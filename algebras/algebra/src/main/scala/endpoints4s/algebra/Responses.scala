package endpoints4s.algebra

import endpoints4s.{InvariantFunctor, InvariantFunctorSyntax, Semigroupal, Tupler}

/** @group algebras
  */
trait Responses extends StatusCodes with InvariantFunctorSyntax {
  self: Errors =>

  /** An HTTP response (status, headers, and entity) carrying an information of type A
    *
    * Values of type `Response[A]` can be constructed by using the operations
    * [[ok]], [[badRequest]], [[internalServerError]], or the more general operation
    * [[response]].
    *
    * @note This type has implicit methods provided by the [[InvariantFunctorSyntax]]
    *       and [[ResponseSyntax]] classes
    * @group types
    */
  type Response[A]

  case class ResponsePayload[BodyP, HeadersP](
      statusCode: StatusCode,
      entity: ResponseEntity[BodyP],
      docs: Documentation = None,
      headers: ResponseHeaders[HeadersP]
  ) {
    def materialize[Out](implicit tupler: Tupler.Aux[BodyP, HeadersP, Out]): Response[Out] =
      self.materialize(this)

    def mapEntity[BodyP2](
        f: ResponseEntity[BodyP] => ResponseEntity[BodyP2]
    ): ResponsePayload[BodyP2, HeadersP] = this.copy(entity = f(entity))

    def mapHeaders[HeadersP2](
        f: ResponseHeaders[HeadersP] => ResponseHeaders[HeadersP2]
    ): ResponsePayload[BodyP, HeadersP2] = this.copy(headers = f(headers))
  }

  implicit def materialize[Out, BodyP, HeadersP](
      payload: ResponsePayload[BodyP, HeadersP]
  )(implicit tupler: Tupler.Aux[BodyP, HeadersP, Out]): Response[Out]

  /** Provides the operation `xmap` to the type `Response`
    * @see [[InvariantFunctorSyntax]]
    */
  implicit def responseInvariantFunctor: InvariantFunctor[Response]

  /** An HTTP response entity carrying an information of type A
    *
    * Values of type [[ResponseEntity]] can be constructed by using the operations
    * [[emptyResponse]] or [[textResponse]]. Additional types of response entities are
    * provided by other algebra modules, such as [[endpoints4s.algebra.JsonEntities JsonEntities]]
    * or [[endpoints4s.algebra.ChunkedEntities ChunkedEntities]].
    *
    * @note This type has implicit methods provided by the [[InvariantFunctorSyntax]]
    *       class
    * @group types
    */
  type ResponseEntity[A]

  implicit def responseEntityInvariantFunctor: InvariantFunctor[ResponseEntity]

  /** Empty response entity
    *
    *   - Server interpreters produce no response entity,
    *   - Client interpreters ignore the response entity.
    *
    * @group operations
    */
  def emptyResponse: ResponseEntity[Unit]

  /** Text response entity
    *
    *   - Server interpreters produce an HTTP response with a `text/plain` content type.
    *
    * @group operations
    */
  def textResponse: ResponseEntity[String]

  /** Information carried by responses’ headers.
    *
    * You can construct values of type `ResponseHeaders` by using the operations [[responseHeader]],
    * [[optResponseHeader]], or [[emptyResponseHeaders]].
    *
    * @note This type has implicit methods provided by the [[SemigroupalSyntax]]
    *       and [[InvariantFunctorSyntax]] classes.
    * @group types
    */
  type ResponseHeaders[A]

  /** Provides `++` operation.
    * @see [[SemigroupalSyntax]]
    */
  implicit def responseHeadersSemigroupal: Semigroupal[ResponseHeaders]

  /** Provides `xmap` operation.
    * @see [[InvariantFunctorSyntax]]
    */
  implicit def responseHeadersInvariantFunctor: InvariantFunctor[ResponseHeaders]

  /** No particular response header.
    *
    *   - Client interpreters should ignore information carried by response headers.
    *
    * @group operations
    */
  def emptyResponseHeaders: ResponseHeaders[Unit]

  /** Response headers containing a header with the given `name`.
    *
    *   - Client interpreters should model the header value as `String`, or
    *    fail if the response header is missing.
    *   - Server interpreters should produce such a response header.
    *   - Documentation interpreters should document this header.
    *
    * Example:
    *
    * {{{
    *   val versionedResource: Endpoint[Unit, (SomeResource, String)] =
    *     endpoint(
    *       get(path / "versioned-resource"),
    *       ok(
    *         jsonResponse[SomeResource],
    *         headers = responseHeader("ETag")
    *       )
    *     )
    * }}}
    *
    * @group operations
    */
  def responseHeader(
      name: String,
      docs: Documentation = None
  ): ResponseHeaders[String]

  /** Response headers optionally containing a header with the given `name`.
    *
    *   - Client interpreters should model the header value as `Some[String]`, or
    *     `None` if the response header is missing.
    *   - Server interpreters should produce such a response header.
    *   - Documentation interpreters should document this header.
    *
    * @group operations
    */
  def optResponseHeader(
      name: String,
      docs: Documentation = None
  ): ResponseHeaders[Option[String]]

  /** Define an HTTP response
    *
    *   - Server interpreters construct a response with the given status and entity.
    *   - Client interpreters accept a response only if it has a corresponding status code.
    *
    * @param statusCode Response status code
    * @param entity     Response entity
    * @param docs       Response documentation
    * @param headers    Response headers
    * @group operations
    */
  def response[A, B](
      statusCode: StatusCode,
      entity: ResponseEntity[A],
      docs: Documentation = None,
      headers: ResponseHeaders[B] = emptyResponseHeaders
  ): ResponsePayload[A, B]

  /** Alternative between two possible choices of responses.
    *
    * Server interpreters construct either one or the other response.
    * Client interpreters accept either one or the other response.
    * Documentation interpreters list all the possible responses.
    */
  def choiceResponse[A, B](
      responseA: Response[A],
      responseB: Response[B]
  ): Response[Either[A, B]]

  /** OK (200) Response with the given entity
    * @group operations
    */
  final def ok[A, B](
      entity: ResponseEntity[A],
      docs: Documentation = None,
      headers: ResponseHeaders[B] = emptyResponseHeaders
  ): ResponsePayload[A, B] = response(OK, entity, docs, headers)

  /** Bad Request (400) response, with an entity of type `ClientErrors`.
    *
    * @see [[endpoints4s.algebra.Errors]] and [[endpoints4s.algebra.BuiltInErrors]]
    * @group operations
    */
  final def badRequest[A](
      docs: Documentation = None,
      headers: ResponseHeaders[A] = emptyResponseHeaders
  ): ResponsePayload[ClientErrors, A] =
    response(BadRequest, clientErrorsResponseEntity, docs, headers)

  /** Internal Server Error (500) response, with an entity of type `ServerError`.
    *
    * @see [[endpoints4s.algebra.Errors]] and [[endpoints4s.algebra.BuiltInErrors]]
    * @group operations
    */
  final def internalServerError[A](
      docs: Documentation = None,
      headers: ResponseHeaders[A] = emptyResponseHeaders
  ): ResponsePayload[ServerError, A] =
    response(InternalServerError, serverErrorResponseEntity, docs, headers)

  /** Turns a `Response[A]` into a `Response[Option[A]]`.
    *
    * Interpreters represent `None` with
    * an empty HTTP response whose status code is 404 (Not Found).
    *
    * @group operations
    */
  final def wheneverFound[A](
      responseA: Response[A],
      notFoundDocs: Documentation = None
  ): Response[Option[A]] =
    responseA
      .orElse(response(NotFound, emptyResponse, notFoundDocs))
      .xmap(_.fold[Option[A]](Some(_), _ => None))(_.toLeft(()))

  /** Extension methods for [[Response]].
    *
    * @group operations
    */
  implicit class ResponseSyntax[A](response: Response[A]) {

    /** Turns a `Response[A]` into a `Response[Option[A]]`.
      *
      * Interpreters represent `None` with
      * an empty HTTP response whose status code is 404 (Not Found).
      */
    final def orNotFound(
        notFoundDocs: Documentation = None
    ): Response[Option[A]] = wheneverFound(response, notFoundDocs)

    /** Alternative between two possible choices of responses.
      *
      * Server interpreters construct either one or the other response.
      * Client interpreters accept either one or the other response.
      * Documentation interpreters list all the possible responses.
      */
    final def orElse[B](otherResponse: Response[B]): Response[Either[A, B]] =
      choiceResponse(response, otherResponse)
  }

}
