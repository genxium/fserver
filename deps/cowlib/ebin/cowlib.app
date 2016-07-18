{application, cowlib, [
	{description, "Support library for manipulating Web protocols."},
	{vsn, "1.3.0"},
	{id, "1.3.0-18-g41949dd-dirty"},
	{modules, ['cow_cookie','cow_date','cow_hpack','cow_http','cow_http2','cow_http_hd','cow_http_te','cow_mimetypes','cow_multipart','cow_qs','cow_spdy','cow_uri','cow_ws']},
	{registered, []},
	{applications, [kernel,stdlib,crypto]}
]}.