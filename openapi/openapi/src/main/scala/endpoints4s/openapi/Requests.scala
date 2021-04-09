package endpoints4s
package openapi

import endpoints4s.algebra.Documentation
import endpoints4s.openapi.model.{MediaType, Schema}

/** Interpreter for [[algebra.Requests]].
  *
  * @group interpreters
  */
trait Requests extends algebra.Requests with Urls with Methods with Headers {

  type RequestHeaders[A] = DocumentedHeaders

  def emptyRequestHeaders = DocumentedHeaders(Nil)

  def requestHeader(name: String, docs: Documentation): RequestHeaders[String] =
    DocumentedHeaders(
      List(DocumentedHeader(name, docs, required = true, Schema.simpleString))
    )

  def optRequestHeader(
      name: String,
      docs: Documentation
  ): RequestHeaders[Option[String]] =
    DocumentedHeaders(
      List(DocumentedHeader(name, docs, required = false, Schema.simpleString))
    )

  type Request[A] = DocumentedRequest

  def materialize[Out, UrlP, BodyP, HeadersP, UrlAndBodyPTupled](
      payload: RequestPayload[UrlP, BodyP, HeadersP]
  )(implicit
      tuplerUB: Tupler.Aux[UrlP, BodyP, UrlAndBodyPTupled],
      tuplerUBH: Tupler.Aux[UrlAndBodyPTupled, HeadersP, Out]
  ): Request[Out] = DocumentedRequest(payload.method, payload.url, payload.headers, payload.docs, payload.entity)

  case class DocumentedRequest(
      method: Method,
      url: DocumentedUrl,
      headers: DocumentedHeaders,
      documentation: Documentation,
      entity: Map[String, MediaType]
  )

  type RequestEntity[A] = Map[String, MediaType]

  lazy val emptyRequest = Map.empty[String, MediaType]

  lazy val textRequest = Map(
    "text/plain" -> MediaType(Some(Schema.simpleString))
  )

  def choiceRequestEntity[A, B](
      requestEntityA: Map[String, MediaType],
      requestEntityB: Map[String, MediaType]
  ): Map[String, MediaType] =
    requestEntityB ++ requestEntityA

  implicit def requestPartialInvariantFunctor: PartialInvariantFunctor[Request] =
    new PartialInvariantFunctor[Request] {
      def xmapPartial[A, B](
          fa: Request[A],
          f: A => Validated[B],
          g: B => A
      ): Request[B] = fa
    }

  implicit lazy val requestEntityPartialInvariantFunctor
      : endpoints4s.PartialInvariantFunctor[RequestEntity] =
    new PartialInvariantFunctor[RequestEntity] {
      def xmapPartial[From, To](
          x: RequestEntity[From],
          map: From => Validated[To],
          contramap: To => From
      ): RequestEntity[To] = x
    }
  implicit lazy val requestHeadersPartialInvariantFunctor
      : endpoints4s.PartialInvariantFunctor[RequestHeaders] =
    new PartialInvariantFunctor[RequestHeaders] {
      def xmapPartial[From, To](
          x: RequestHeaders[From],
          map: From => Validated[To],
          contramap: To => From
      ): RequestHeaders[To] = x
    }
  implicit lazy val requestHeadersSemigroupal: endpoints4s.Semigroupal[RequestHeaders] =
    new Semigroupal[RequestHeaders] {
      def product[A, B](fa: RequestHeaders[A], fb: RequestHeaders[B])(implicit
          tupler: Tupler[A, B]
      ): RequestHeaders[tupler.Out] =
        DocumentedHeaders(fa.value ++ fb.value)
    }

}
