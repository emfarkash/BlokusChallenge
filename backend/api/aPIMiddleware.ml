open Opium.Std
open Lwt.Infix

(** [cors_origin_middleware] is a middleware function for Opium that handles 
    CORS request origin for the API *)
let cors_origin_middleware =
  let filter handler req =
    handler req >|= fun response -> 
    let headers = 
      Cohttp.Header.add 
        (Rock.Response.headers response)
        "Access-Control-Allow-Origin"
        "http://localhost:3000" 
    in
    { response with headers }
  in 
  Rock.Middleware.create ~name:"middleware" ~filter

(** [cors_method_middleware] is a middleware function for Opium that handles 
    CORS method type for the API *)
let cors_method_middleware =
  let filter handler req =
    handler req >|= fun response -> 
    let headers = 
      Cohttp.Header.add 
        (Rock.Response.headers response)
        "Access-Control-Allow-Methods"
        "GET, POST, OPTIONS"
    in
    { response with headers }
  in 
  Rock.Middleware.create ~name:"middleware" ~filter

(** [cors_headers_middlemare] is a middleware function for Opium that handles
    CORS headers for the API *)
let cors_headers_middleware = 
  let filter handler req =
    handler req >|= fun response -> 
    let headers = 
      Cohttp.Header.add 
        (Rock.Response.headers response) 
        "Access-Control-Allow-Headers"
        "Origin, Content-Type, Accept"
    in
    { response with headers }
  in 
  Rock.Middleware.create ~name:"middleware" ~filter