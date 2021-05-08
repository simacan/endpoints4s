package endpoints4s.akkahttp.server

import akka.http.scaladsl.model.headers.{
  Authorization,
  BasicHttpCredentials,
  HttpChallenges,
  `WWW-Authenticate`
}
import akka.http.scaladsl.model.{HttpHeader, HttpResponse, Uri, StatusCodes => AkkaStatusCodes}
import akka.http.scaladsl.server.{Directive, Directive1, Directives}
import endpoints4s.{Tupler, Valid, Validated, algebra}
import endpoints4s.algebra.BasicAuthentication.Credentials
import endpoints4s.algebra.Documentation

/** @group interpreters
  */
trait BasicAuthentication extends algebra.BasicAuthentication with EndpointsWithCustomErrors {

  //def authHeaders[H, HCred](headers: RequestHeaders[H])(implicit
  //    tuplerHCred: Tupler.Aux[H, Credentials, HCred]
  //): RequestHeaders[HCred] = {
  //  val authHeader: RequestHeaders[Option[Credentials]] =
  //    httpHeaders =>
  //      Valid(
  //        httpHeaders.header[Authorization].flatMap {
  //          case Authorization(BasicHttpCredentials(username, password)) =>
  //            Some(Credentials(username, password))
  //          case _ => None
  //        }
  //      )
  //  (headers ++ authHeader).xmapPartial({
  //    case (h, Some(credentials)) =>
  //      Valid(tuplerHCred(h, credentials))
  //    case (h, None) =>
  //      Invalid("")
  //  })({ hCred =>
  //    val (h, cred) = tuplerHCred.unapply(hCred)
  //    (h, Some(cred))
  //  })
  //}

  //def authRequest[A, Out](request: Request[A]): Request[Out] = {
  //  request.copy(headers = authHeaders(request.headers))
  //}

  private[endpoints4s] def authenticatedRequest[U, E, H, UE, HCred, Out](
      method: Method,
      url: Url[U],
      entity: RequestEntity[E],
      headers: RequestHeaders[H],
      requestDocs: Documentation
  )(implicit
      tuplerUE: Tupler.Aux[U, E, UE],
      tuplerHCred: Tupler.Aux[H, Credentials, HCred],
      tuplerUEHCred: Tupler.Aux[UE, HCred, Out]
  ): Request[Out] = {
    val _method = method
    val _url = url
    val _entity = entity
    val _docs = requestDocs
    val _headers = headers
    new Request[Out] {
      type UrlP = U
      type EntityP = E
      type HeadersP = H
      val authHeader: RequestHeaders[Option[Credentials]] =
        httpHeaders =>
          Valid(
            httpHeaders.header[Authorization].flatMap {
              case Authorization(BasicHttpCredentials(username, password)) =>
                Some(Credentials(username, password))
              case _ => None
            }
          )

      val headersDirective: Directive1[HCred] =
        // First, handle regular header validation
        directive1InvFunctor
          .xmapPartial[Validated[(H, Option[Credentials])], (H, Option[Credentials])](
            Directives.extractRequest.map((headers ++ authHeader).decode),
            validated => validated,
            headersAndCredentials => Valid(headersAndCredentials)
          )
          .flatMap {
            // Then, check that credentials are present
            case (h, Some(credentials)) =>
              Directives.provide(tuplerHCred(h, credentials))
            case (_, None) =>
              Directive[Tuple1[HCred]] { _ => //inner is ignored
                Directives.complete(
                  HttpResponse(
                    AkkaStatusCodes.Unauthorized,
                    collection.immutable.Seq[HttpHeader](
                      `WWW-Authenticate`(HttpChallenges.basic("Realm"))
                    )
                  )
                )
              }
          }

      val directive =
        joinDirectives(
          joinDirectives(
            joinDirectives(
              convToDirective1(Directives.method(method)),
              url.directive
            ),
            entity
          ),
          headersDirective
        )

      def uri(out: Out): Uri = {
        val (ue, _) = tuplerUEHCred.unapply(out)
        val (u, _) = tuplerUE.unapply(ue)
        url.uri(u)
      }
      def method: Method = _method
      def url: Url[UrlP] = _url
      def entity: RequestEntity[EntityP] = _entity
      def headers: RequestHeaders[HeadersP] = _headers
      def documentation: Documentation = _docs
    }
  }

}
