open Angstrom

(* headers and columns *)
let ledes1998b_header = "LEDES1998B[]"

let ledes1998b_data_columns =
  "INVOICE_DATE|INVOICE_NUMBER|CLIENT_ID|LAW_FIRM_MATTER_ID|INVOICE_TOTAL|BILLING_START_DATE|BILLING_END_DATE|INVOICE_DESCRIPTION|LINE_ITEM_NUMBER|EXP/FEE/INV_ADJ_TYPE|LINE_ITEM_NUMBER_OF_UNITS|LINE_ITEM_ADJUSTMENT_AMOUNT|LINE_ITEM_TOTAL|LINE_ITEM_DATE|LINE_ITEM_TASK_CODE|LINE_ITEM_EXPENSE_CODE|LINE_ITEM_ACTIVITY_CODE|TIMEKEEPER_ID|LINE_ITEM_DESCRIPTION|LAW_FIRM_ID|LINE_ITEM_UNIT_COST|TIMEKEEPER_NAME|TIMEKEEPER_CLASSIFICATION|CLIENT_MATTER_ID[]"

let ledes98bi_header = "LEDES98BI V2[]"

let ledes98bi_data_columns =
  "INVOICE_DATE|INVOICE_NUMBER|CLIENT_ID|LAW_FIRM_MATTER_ID|INVOICE_TOTAL|BILLING_START_DATE|BILLING_END_DATE|INVOICE_DESCRIPTION|LINE_ITEM_NUMBER|EXP/FEE/INV_ADJ_TYPE|LINE_ITEM_NUMBER_OF_UNITS|LINE_ITEM_ADJUSTMENT_AMOUNT|LINE_ITEM_TOTAL|LINE_ITEM_DATE|LINE_ITEM_TASK_CODE|LINE_ITEM_EXPENSE_CODE|LINE_ITEM_ACTIVITY_CODE|TIMEKEEPER_ID|LINE_ITEM_DESCRIPTION|LAW_FIRM_ID|LINE_ITEM_UNIT_COST|TIMEKEEPER_NAME|TIMEKEEPER_CLASSIFICATION|CLIENT_MATTER_ID|PO_NUMBER|CLIENT_TAX_ID|MATTER_NAME|INVOICE_TAX_TOTAL|INVOICE_NET_TOTAL|INVOICE_CURRENCY|TIMEKEEPER_LAST_NAME|TIMEKEEPER_FIRST_NAME|ACCOUNT_TYPE|LAW_FIRM_NAME|LAW_FIRM_ADDRESS_1|LAW_FIRM_ADDRESS_2|LAW_FIRM_CITY|LAW_FIRM_STATEorREGION|LAW_FIRM_POSTCODE|LAW_FIRM_COUNTRY|CLIENT_NAME|CLIENT_ADDRESS_1|CLIENT_ADDRESS_2|CLIENT_CITY|CLIENT_STATEorREGION|CLIENT_POSTCODE|CLIENT_COUNTRY|LINE_ITEM_TAX_RATE|LINE_ITEM_TAX_TOTAL|LINE_ITEM_TAX_TYPE|INVOICE_REPORTED_TAX_TOTAL|INVOICE_TAX_CURRENCY[]"

(* data validators *)
let is_valid_date s =
  if String.length s <> 8 then false
  else
    let y = int_of_string_opt (String.sub s 0 4)
    and m = int_of_string_opt (String.sub s 4 2)
    and d = int_of_string_opt (String.sub s 6 2) in
    match (y, m, d) with
    | Some y, Some m, Some d ->
        Ptime.of_date_time ((y, m, d), ((0, 0, 0), 0)) <> None
    | _ -> false

let is_valid_char_field max_len s = String.length s <= max_len

let is_valid_alphanumeric max_len s =
  Re.execp (Re.Emacs.compile (Re.Emacs.re "^[A-Za-z0-9]*$")) s
  && String.length s <= max_len

let is_valid_kb_size max_kb s =
  Bytes.length (Bytes.of_string s) <= max_kb * 1024

let is_valid_currency max_digits max_decimals s =
  let pattern =
    Printf.sprintf "^-?[0-9]{1,%d}(\\.[0-9]{0,%d})?$" max_digits max_decimals
  in
  let re = Re.Perl.compile (Re.Perl.re pattern) in
  Re.execp re s

let is_valid_float max_digits max_decimals =
  is_valid_currency max_digits max_decimals

let is_valid_line_item_cost f s =
  if s = "0" || s = "" then List.mem f [ "IF"; "IE" ]
  else s <> "" && is_valid_float 10 4 s

let is_valid_timekeeper_classification s =
  List.mem s
    [
      "ADJSTR";
      "ADJSMT";
      "ANALST";
      "ARBITR";
      "ASSOC";
      "TRANEE";
      "AUDITR";
      "CLKSEC";
      "CONSLT";
      "TEMPAT";
      "TEMPOS";
      "CSTCNS";
      "CTRPTR";
      "DSCATT";
      "DSCPMG";
      "DSCANJ";
      "DSCANS";
      "DOCCDR";
      "DOCRAT";
      "DOCRNA";
      "EVDEVL";
      "EXPERT";
      "FLTFEE";
      "FORANL";
      "HLNLPF";
      "INVSTG";
      "IPSVPV";
      "IPAGNT";
      "LGLAST";
      "LGLINT";
      "LGPRMG";
      "LIBRRN";
      "MEDITR";
      "MDRCRV";
      "OFCOUN";
      "NBOTHR";
      "PARALG";
      "PARTNR";
      "RSRCHR";
      "RSPROF";
      "SECNDE";
      "SPAGNT";
      "SOLDSG";
      "STFATT";
      "STFCNS";
      "TPADMN";
      "TRAINR";
      "TRNDEV";
      "TRNFAG";
      "TRANSL";
      "VIDOGR";
    ]

let is_valid_iso4217_code s =
  List.mem s
    [
      "AED";
      "AFN";
      "ALL";
      "AMD";
      "ANG";
      "AOA";
      "ARS";
      "AUD";
      "AWG";
      "AZN";
      "BAM";
      "BBD";
      "BDT";
      "BGN";
      "BHD";
      "BIF";
      "BMD";
      "BND";
      "BOB";
      "BOV";
      "BRL";
      "BSD";
      "BTN";
      "BWP";
      "BYN";
      "BZD";
      "CAD";
      "CDF";
      "CHE";
      "CHF";
      "CHW";
      "CLF";
      "CLP";
      "CNY";
      "COP";
      "COU";
      "CRC";
      "CUP";
      "CVE";
      "CZK";
      "DJF";
      "DKK";
      "DOP";
      "DZD";
      "EGP";
      "ERN";
      "ETB";
      "EUR";
      "FJD";
      "FKP";
      "GBP";
      "GEL";
      "GHS";
      "GIP";
      "GMD";
      "GNF";
      "GTQ";
      "GYD";
      "HKD";
      "HNL";
      "HRK";
      "HTG";
      "HUF";
      "IDR";
      "ILS";
      "INR";
      "IQD";
      "IRR";
      "ISK";
      "JMD";
      "JOD";
      "JPY";
      "KES";
      "KGS";
      "KHR";
      "KMF";
      "KPW";
      "KRW";
      "KWD";
      "KYD";
      "KZT";
      "LAK";
      "LBP";
      "LKR";
      "LRD";
      "LSL";
      "LYD";
      "MAD";
      "MDL";
      "MGA";
      "MKD";
      "MMK";
      "MNT";
      "MOP";
      "MRU";
      "MUR";
      "MVR";
      "MWK";
      "MXN";
      "MXV";
      "MYR";
      "MZN";
      "NAD";
      "NGN";
      "NIO";
      "NOK";
      "NPR";
      "NZD";
      "OMR";
      "PAB";
      "PEN";
      "PGK";
      "PHP";
      "PKR";
      "PLN";
      "PYG";
      "QAR";
      "RON";
      "RSD";
      "RUB";
      "RWF";
      "SAR";
      "SBD";
      "SCR";
      "SDG";
      "SEK";
      "SGD";
      "SHP";
      "SLE";
      "SOS";
      "SRD";
      "SSP";
      "STN";
      "SVC";
      "SYP";
      "SZL";
      "THB";
      "TJS";
      "TMT";
      "TND";
      "TOP";
      "TRY";
      "TTD";
      "TWD";
      "TZS";
      "UAH";
      "UGX";
      "USD";
      "USN";
      "UYI";
      "UYU";
      "UYW";
      "UZS";
      "VED";
      "VES";
      "VND";
      "VUV";
      "WST";
      "XAF";
      "XAG";
      "XAU";
      "XBA";
      "XBB";
      "XBC";
      "XBD";
      "XCD";
      "XCG";
      "XDR";
      "XOF";
      "XPD";
      "XPF";
      "XPT";
      "XSU";
      "XTS";
      "XUA";
      "XXX";
      "YER";
      "ZAR";
      "ZMW";
      "ZWG";
    ]

let is_valid_timekeep_name tkn lic s =
  s <> "" && (tkn <> "" || lic = "F" || lic = "IF")

let is_valid_account_type s = s = "O" || s = "T"

let is_valid_decimal s =
  let is_valid_format =
    try
      let f = float_of_string s in
      f >= 0.0 && f < 1.0
    with Failure _ -> false
  in
  if not is_valid_format then false
  else
    match String.split_on_char '.' s with
    | [ _ ] -> true
    | [ _; frac ] -> String.length frac <= 10
    | _ -> false

(* data parsers and utilities *)
let flip f x y = f y x

let rec remaining n lst =
  match (n, lst) with
  | 0, _ -> lst
  | _, [] -> []
  | n, _ :: tl -> remaining (n - 1) tl

let get_index fields n = List.nth fields (n - 1)
let bracket = string "[]" <?> "Bracket '[]' is missing"
let data_field = take_while (fun c -> c <> '|')
let last_data_field = take_while (fun c -> c <> '[')
let pipe = char '|' <?> "Pipe '|' is missing"
let valid_exp_fee_inv_adi_type = [ "E"; "F"; "IF"; "IE" ]
let data_field_parser = data_field <* pipe
let last_data_field_parser = last_data_field <* bracket

let ledes1998b_header_parser =
  string ledes1998b_header <?> "Line 1: Invalid or missing LEDES1998B header"

let ledes1998b_column_parser =
  string ledes1998b_data_columns
  <?> "Line 2: Invalid or missing LEDES1998B column definition"

let ledes98bi_header_parser =
  string ledes98bi_header <?> "Line 1: Invalid or missing LEDES98BI header"

let ledes98bi_column_parser =
  string ledes98bi_data_columns
  <?> "Line 2: Invalid or missing LEDES98BI column definition"

(* greedy *)
let data_field_validate_greedy data errors ~line_num ~field_num ~field_name
    ~pred ~required =
  if (required && data = "") || (data <> "" && not (pred data)) then
    Printf.sprintf "Line %d : At field %s : Invalid %s" line_num field_num
      field_name
    :: errors
  else errors

let data_field_validate_with_dep_greedy data errors ~line_num ~field_num
    ~field_name ~pred =
  if not (pred data) then
    Printf.sprintf "Line %d : At field %s : Invalid %s" line_num field_num
      field_name
    :: errors
  else errors

let validate_ledes1998b_header_greedy errors s =
  if s = ledes1998b_header then errors
  else "Line 1: Invalid or missing LEDES1998B header" :: errors

let validate_ledes1998b_data_columns_greedy errors s =
  if s = ledes1998b_data_columns then errors
  else "Line 2: Invalid or missing LEDES1998B column definition" :: errors

let validate_ledes98bi_header_greedy errors s =
  if s = ledes98bi_header then errors
  else "Line 1: Invalid or missing LEDES98BI header" :: errors

let validate_ledes98bi_data_columns_greedy errors s =
  if s = ledes98bi_data_columns then errors
  else "Line 2: Invalid or missing LEDES98BI column definition" :: errors

let parse_and_validate_line_greedy errors line_parser_greedy
    fields_validator_greedy ~line_num line =
  match parse_string ~consume:All line_parser_greedy line with
  | Ok fields -> fields_validator_greedy errors fields line_num
  | Error msg -> msg :: errors

let parse_and_validate_greedy_inner ~header_validator ~column_validator
    ~data_line_parser_greedy ~data_fields_validator_greedy lines =
  let len = List.length lines in
  let errors = [] in
  if len < 3 then Printf.sprintf "A valid file is at least 3 lines" :: errors
  else
    let hrd_errs = header_validator errors (List.nth lines 0) in
    let col_errs = column_validator errors (List.nth lines 1) in
    let data_errs =
      List.mapi
        (fun i line ->
          parse_and_validate_line_greedy errors data_line_parser_greedy
            data_fields_validator_greedy ~line_num:(i + 3) line)
        (remaining 2 lines)
      |> List.concat
    in
    let err = hrd_errs @ col_errs @ data_errs in
    if err = [] then [ "" ] else err

(* eager *)
let data_field_parser_eager ~field_num ~field_name ~pred ~required =
  data_field_parser >>= fun data ->
  if (required && data = "") || (data <> "" && not (pred data)) then
    fail (Printf.sprintf "At field %s : Invalid %s" field_num field_name)
  else return data

let data_field_parser_with_dep_eager ~field_num ~field_name ~pred =
  data_field_parser >>= fun data ->
  if not (pred data) then
    fail (Printf.sprintf "At field %s : Invalid %s" field_num field_name)
  else return data

let parse_and_validate_line_eager parser line =
  match parse_string ~consume:Consume.All parser line with
  | Ok _ -> Ok ()
  | Error msg -> Error (Printf.sprintf "%s" msg)

let last_data_field_parser_eager ~field_num ~field_name ~pred ~required =
  last_data_field_parser >>= fun data ->
  if (required && data = "") || (data <> "" && not (pred data)) then
    fail (Printf.sprintf "At last field %s : Invalid %s" field_num field_name)
  else return data

let parse_and_validate_data_lines_eager data_parser lines =
  let rec aux lineno acc = function
    | [] -> Ok []
    | line :: rest -> (
        match parse_string ~consume:Consume.All data_parser line with
        | Ok parsed -> aux (lineno + 1) (parsed :: acc) rest
        | Error msg -> Error (Printf.sprintf "Line %d %s" (lineno + 3) msg))
  in
  aux 0 [] lines

let parse_and_validate_eager_inner lines ~header_parser_eager
    ~column_parser_eager ~data_parser =
  if List.length lines < 3 then Printf.printf "A valid file is at least 3 lines"
  else
    let header_line =
      parse_and_validate_line_eager header_parser_eager (List.nth lines 0)
    in
    let column_line =
      parse_and_validate_line_eager column_parser_eager (List.nth lines 1)
    in
    if Result.is_error header_line then
      Printf.printf "%s\n" (Result.get_error header_line)
    else if Result.is_error column_line then
      Printf.printf "%s\n" (Result.get_error column_line)
    else
      match
        parse_and_validate_data_lines_eager data_parser (remaining 2 lines)
      with
      | Ok parsed ->
          List.iter (fun _ -> ()) parsed;
          Printf.printf "\n"
      | Error msg -> Printf.printf "%s\n" msg
