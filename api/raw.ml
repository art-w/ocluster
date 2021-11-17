include Schema.MakeRPC(Capnp_rpc_lwt)

module Internal = Capnp.Runtime.BuilderInc.Make (MessageWrapper)

let init_data pointer_bytes value =
  let open Internal.BA_ in
  let new_string_storage =
    uint8_list_of_string
        ~null_terminated:false
        ~dest_message:pointer_bytes.NM.Slice.msg
        value
  in
  BOps.deep_zero_pointer pointer_bytes ;
  BOps.init_list_pointer pointer_bytes new_string_storage

let deref_data ?(default = "") = function
  | None -> default
  | Some pointer_bytes ->
      let open Internal in
      match RA_.deref_list_pointer pointer_bytes with
      | None -> default
      | Some list_storage ->
          BA_.NC.string_of_uint8_list ~null_terminated:false list_storage
