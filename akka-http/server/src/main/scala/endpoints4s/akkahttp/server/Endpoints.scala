package endpoints4s.akkahttp.server

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpHeader, HttpRequest, MediaTypes, Uri}
import akka.http.scaladsl.server.{Directive1, Directives, ExceptionHandler, Route, StandardRoute}
import akka.http.scaladsl.unmarshalling._
import endpoints4s.algebra.Documentation
import endpoints4s.{
  Invalid,
  InvariantFunctor,
  PartialInvariantFunctor,
  Semigroupal,
  Tupler,
  Valid,
  Validated,
  algebra
}

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/** Interpreter for [[algebra.Endpoints]] that performs routing using Akka-HTTP and uses [[algebra.BuiltInErrors]]
  * to model client and server errors.
  *
  * @group interpreters
  */
trait Endpoints extends algebra.Endpoints with EndpointsWithCustomErrors with BuiltInErrors

/** Interpreter for [[algebra.Endpoints]] that performs routing using Akka-HTTP.
  * @group interpreters
  */
trait EndpointsWithCustomErrors
    extends algebra.EndpointsWithCustomErrors
    with Urls
    with Methods
    with StatusCodes {

  trait RequestHeaders[A] {
    def decode(httpRequest: HttpRequest): Validated[A]
  }

  trait Request[A] {

    /** A directive that extracts an `A` from an incoming request */
    def directive: Directive1[A]

    /** The URI of a request carrying the given `a` parameter */
    def uri(a: A): Uri
  }

  type RequestEntity[A] = Directive1[A]

  type ResponseEntity[A] = ToEntityMarshaller[A]

  type ResponseHeaders[A] = A => collection.immutable.Seq[HttpHeader]

  type Response[A] = A => Route

  def materialize[Out, UrlP, BodyP, HeadersP, UrlAndBodyPTupled](
      payload: RequestPayload[UrlP, BodyP, HeadersP]
  )(implicit
      tuplerUB: Tupler.Aux[UrlP, BodyP, UrlAndBodyPTupled],
      tuplerUBH: Tupler.Aux[UrlAndBodyPTupled, HeadersP, Out]
  ): Request[Out] = new Request[Out] {
    val directive = {
      val methodDirective = convToDirective1(Directives.method(payload.method))
      val headersDirective: Directive1[HeadersP] =
        directive1InvFunctor.xmapPartial[Validated[HeadersP], HeadersP](
          Directives.extractRequest.map(payload.headers.decode),
          identity,
          c => Valid(c)
        )
      val matchDirective = methodDirective & payload.url.directive & headersDirective
      matchDirective.tflatMap { case (_, a, c) =>
        payload.entity.map(b => tuplerUBH(tuplerUB(a, b), c))
      }
    }
    def uri(out: Out): Uri = {
      val (ab, _) = tuplerUBH.unapply(out)
      val (a, _) = tuplerUB.unapply(ab)
      payload.url.uri(a)
    }
  }

  implicit def materialize[Out, EntityP, HeadersP](payload: ResponsePayload[EntityP, HeadersP])(
      implicit tupler: Tupler.Aux[EntityP, HeadersP, Out]
  ): Response[Out] =
    r => {
      val (a, b) = tupler.unapply(r)
      val httpHeaders = payload.headers(b)
      implicit val marshaller: ToResponseMarshaller[A] =
        Marshaller.fromToEntityMarshaller(payload.statusCode, payload.httpHeaders)(payload.entity)
      Directives.complete(a)
    }

  implicit def materialize[
      A,
      B,
      UrlP,
      RequestBodyP,
      RequestHeadersP,
      UrlAndRequestBodyPTupled,
      ResponseBodyP,
      ResponseHeadersP
  ](
      payload: EndpointPayload[UrlP, RequestBodyP, RequestHeadersP, ResponseBodyP, ResponseHeadersP]
  )(implicit
      tuplerUB: Tupler.Aux[UrlP, RequestBodyP, UrlAndRequestBodyPTupled],
      tuplerUBH: Tupler.Aux[UrlAndRequestBodyPTupled, RequestHeadersP, A],
      tuplerBH: Tupler.Aux[ResponseBodyP, ResponseHeadersP, B]
  ): Endpoint[A, B] = Endpoint(payload.request.materialize, payload.response.materialize)

  implicit lazy val responseInvariantFunctor: InvariantFunctor[Response] =
    new InvariantFunctor[Response] {
      def xmap[A, B](
          fa: Response[A],
          f: A => B,
          g: B => A
      ): Response[B] =
        fa compose g
    }

  implicit lazy val responseEntityInvariantFunctor: InvariantFunctor[ResponseEntity] =
    new InvariantFunctor[ResponseEntity] {
      def xmap[A, B](
          fa: ResponseEntity[A],
          f: A => B,
          g: B => A
      ): ResponseEntity[B] = fa compose g
    }

  private[server] val endpointsExceptionHandler =
    ExceptionHandler { case NonFatal(t) => handleServerError(t) }

  case class Endpoint[A, B](request: Request[A], response: Response[B]) {

    /** @return An Akka HTTP `Route` for this endpoint
      * @param implementation Function that transforms the `A` value carried in
      *                       the request into a `B` value to send in the response.
      */
    def implementedBy(implementation: A => B): Route =
      Directives.handleExceptions(endpointsExceptionHandler) {
        request.directive { arguments =>
          Directives.encodeResponse {
            response(implementation(arguments))
          }
        }
      }

    /** @return An Akka HTTP `Route` for this endpoint
      * @param implementation Asynchronous function that transforms the `A` value
      *                       carried in the request into a `B` value to send in
      *                       the response.
      */
    def implementedByAsync(implementation: A => Future[B]): Route =
      Directives.handleExceptions(endpointsExceptionHandler) {
        request.directive { arguments =>
          Directives.onComplete(implementation(arguments)) {
            case Success(result) => Directives.encodeResponse(response(result))
            case Failure(ex)     => throw ex
          }
        }
      }

    /** @return The `Uri` of this endpoint, for a request carrying the
      *         given `a` value.
      */
    def uri(a: A): Uri = request.uri(a)
  }

  /* ************************
      REQUESTS
  ************************* */

  implicit def requestPartialInvariantFunctor: PartialInvariantFunctor[Request] =
    new PartialInvariantFunctor[Request] {
      def xmapPartial[A, B](fa: Request[A], f: A => Validated[B], g: B => A): Request[B] =
        new Request[B] {
          val directive: Directive1[B] = directive1InvFunctor.xmapPartial(fa.directive, f, g)
          def uri(b: B): Uri = fa.uri(g(b))
        }
    }

  def emptyRequest: RequestEntity[Unit] = convToDirective1(Directives.pass)

  def textRequest: RequestEntity[String] = {
    implicit val um: FromEntityUnmarshaller[String] =
      Unmarshaller.stringUnmarshaller
        .forContentTypes(MediaTypes.`text/plain`)
    Directives.entity[String](implicitly)
  }

  def choiceRequestEntity[A, B](
      requestEntityA: Directive1[A],
      requestEntityB: Directive1[B]
  ): Directive1[Either[A, B]] = {
    val requestEntityAAsEither = requestEntityA.map(Left(_): Either[A, B])
    val requestEntityBAsEither = requestEntityB.map(Right(_): Either[A, B])

    requestEntityAAsEither | requestEntityBAsEither
  }

  implicit lazy val requestEntityPartialInvariantFunctor: PartialInvariantFunctor[RequestEntity] =
    directive1InvFunctor

  /* ************************
      HEADERS
  ************************* */

  def emptyRequestHeaders: RequestHeaders[Unit] =
    _ => Valid(())

  def requestHeader(name: String, docs: Documentation): RequestHeaders[String] =
    httpRequest =>
      httpRequest.headers.find(_.lowercaseName() == name.toLowerCase) match {
        case Some(header) => Valid(header.value())
        case None         => Invalid(s"Missing header $name")
      }

  def optRequestHeader(
      name: String,
      docs: Documentation
  ): RequestHeaders[Option[String]] =
    httpRequest =>
      httpRequest.headers.find(_.lowercaseName() == name.toLowerCase) match {
        case Some(header) => Valid(Some(header.value()))
        case None         => Valid(None)
      }

  implicit lazy val requestHeadersPartialInvariantFunctor: PartialInvariantFunctor[RequestHeaders] =
    new PartialInvariantFunctor[RequestHeaders] {
      def xmapPartial[A, B](
          fa: RequestHeaders[A],
          f: A => Validated[B],
          g: B => A
      ): RequestHeaders[B] =
        headers => fa.decode(headers).flatMap(f)
    }
  implicit lazy val requestHeadersSemigroupal: Semigroupal[RequestHeaders] =
    new Semigroupal[RequestHeaders] {
      def product[A, B](fa: RequestHeaders[A], fb: RequestHeaders[B])(implicit
          tupler: Tupler[A, B]
      ): RequestHeaders[tupler.Out] =
        (httpRequest: HttpRequest) => fa.decode(httpRequest).zip(fb.decode(httpRequest))(tupler)
    }

  /* ************************
      RESPONSES
  ************************* */

  implicit def responseHeadersSemigroupal: Semigroupal[ResponseHeaders] =
    new Semigroupal[ResponseHeaders] {
      def product[A, B](fa: ResponseHeaders[A], fb: ResponseHeaders[B])(implicit
          tupler: Tupler[A, B]
      ): ResponseHeaders[tupler.Out] =
        out => {
          val (a, b) = tupler.unapply(out)
          fa(a) ++ fb(b)
        }
    }

  implicit def responseHeadersInvariantFunctor: InvariantFunctor[ResponseHeaders] =
    new InvariantFunctor[ResponseHeaders] {
      def xmap[A, B](
          fa: ResponseHeaders[A],
          f: A => B,
          g: B => A
      ): ResponseHeaders[B] =
        fa compose g
    }

  def emptyResponseHeaders: ResponseHeaders[Unit] = _ => Nil

  def responseHeader(
      name: String,
      docs: Documentation = None
  ): ResponseHeaders[String] =
    value => RawHeader(name, value) :: Nil

  def optResponseHeader(
      name: String,
      docs: Documentation = None
  ): ResponseHeaders[Option[String]] = {
    case Some(value) => RawHeader(name, value) :: Nil
    case None        => Nil
  }

  def emptyResponse: ResponseEntity[Unit] =
    Marshaller.opaque(_ => HttpEntity.Empty)

  def textResponse: ResponseEntity[String] = implicitly

  def choiceResponse[A, B](
      responseA: Response[A],
      responseB: Response[B]
  ): Response[Either[A, B]] = {
    case Left(a)  => responseA(a)
    case Right(b) => responseB(b)
  }

  def endpoint[A, B](
      request: Request[A],
      response: Response[B],
      docs: EndpointDocs = EndpointDocs()
  ): Endpoint[A, B] = Endpoint(request, response)

  lazy val directive1InvFunctor: PartialInvariantFunctor[Directive1] =
    new PartialInvariantFunctor[Directive1] {
      def xmapPartial[From, To](
          f: Directive1[From],
          map: From => Validated[To],
          contramap: To => From
      ): Directive1[To] =
        f.flatMap(from =>
          map(from) match {
            case Valid(value)     => Directives.provide(value)
            case invalid: Invalid => handleClientErrors(invalid)
          }
        )
    }

  /** This method is called by ''endpoints'' when an exception is thrown during
    * request processing.
    *
    * The provided implementation uses [[serverErrorResponse]] to complete
    * with a response containing the error message.
    *
    * This method can be overridden to customize the error reporting logic.
    */
  def handleServerError(throwable: Throwable): StandardRoute =
    StandardRoute(serverErrorResponse(throwableToServerError(throwable)))

}
