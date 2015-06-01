open Lwt
open V1_LWT
open Printf

let red fmt    = sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = sprintf ("\027[36m"^^fmt^^"\027[m")

let domain = "anil.recoil.org"
let uri = Uri.of_string "http://anil.recoil.org"
let ns = "8.8.8.8"

module Client (C:CONSOLE) (RES:Resolver_lwt.S) (CON:Conduit_mirage.S) = struct

  module HTTP = Cohttp_mirage.Client

  let http_fetch c resolver ctx =
    C.log_s c (sprintf "Fetching %s with Cohttp:" (Uri.to_string uri)) >>= fun () ->
    let ctx = HTTP.ctx resolver ctx in
    HTTP.get ~ctx uri >>= fun (response, body) ->
    Cohttp_lwt_body.to_string body >>= fun body ->
    C.log_s c (Sexplib.Sexp.to_string_hum (Cohttp.Response.sexp_of_t response)) >>= fun () ->
    C.log_s c (sprintf "Received body length: %d" (String.length body)) >>= fun () ->
    C.log_s c "Cohttp fetch done\n------------\n"

  let start c res ctx =
    C.log_s c (sprintf "Resolving in 1s using DNS server %s" ns) >>= fun () ->
    OS.Time.sleep 1.0 >>= fun () ->
    http_fetch c res ctx 
    
end
