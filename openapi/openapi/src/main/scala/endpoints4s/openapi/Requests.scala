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

  override type Request[A] = DocumentedRequest

  case class DocumentedRequest(
      method: Method,
      url: DocumentedUrl,
      headers: DocumentedHeaders,
      documentation: Documentation,
      entity: DocumentedRequestEntity
  ) {
    type UrlP = DocumentedUrl#UrlP
    type HeadersP = DocumentedHeaders#HeadersP
    type EntityP = DocumentedRequestEntity#EntityP
  }

  type RequestEntity[A] = DocumentedRequestEntity

  case class DocumentedRequestEntity(map: Map[String, MediaType]) {
    type EntityP = Nothing
  }

  lazy val emptyRequest = DocumentedRequestEntity(Map.empty[String, MediaType])

  lazy val textRequest = DocumentedRequestEntity(Map(
    "text/plain" -> MediaType(Some(Schema.simpleString))
  ))

  def choiceRequestEntity[A, B](
      requestEntityA: DocumentedRequestEntity,
      requestEntityB: DocumentedRequestEntity,
  ): DocumentedRequestEntity =
    DocumentedRequestEntity(requestEntityB.map ++ requestEntityA.map)

  def request[A, B, C, AB, Out](
      method: Method,
      url: Url[A],
      entity: RequestEntity[B] = emptyRequest,
      docs: Documentation = None,
      headers: RequestHeaders[C] = emptyRequestHeaders
  )(implicit
      tuplerAB: Tupler.Aux[A, B, AB],
      tuplerABC: Tupler.Aux[AB, C, Out]
  ): Request[Out] =
    DocumentedRequest(method, url, headers, docs, entity)

  implicit def toDocReq(map: Map[String, MediaType]): DocumentedRequestEntity = DocumentedRequestEntity(map)

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
