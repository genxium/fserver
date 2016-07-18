-ifndef(COMMON_HRL).
-define(COMMON_HRL, true).

-define(APP_NAME, 'fakeoauth2server').

-include("define_logger.hrl").
-include("define_error_code.hrl").

-define(RET(__CODE), #{ret => __CODE}).

-endif.
