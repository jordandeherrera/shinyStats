library(httr)

# OAuth 2 example, lifted from https://github.com/hadley/httr/blob/master/demo/oauth2-github.r

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at at
#    https://github.com/settings/applications. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("JDTest",
                   key = "5d48cf474a0ae10d30e7",
                   secret = "91c7e051ebfc79502fd19786d2bb217a4441d5e6",
                   redirect_uri = "http://localhost:1410/"
)

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"),
                               myapp, cache = TRUE)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
stop_for_status(req)
rate_limit <- content(req)
rate_limit$resources$core$limit

# See API documentation for all resources, examples


library(httr)
library(httpuv)

endPoint <- oauth_endpoint(#request = NULL,
  authorize = "https://appcenter.intuit.com/connect/oauth2",
  access = "https://oauth.platform.intuit.com/oauth2/v1/tokens/bearer")
App <- oauth_app("JDTest",
                 key = "Q0ynnhALzOpIzvI8iZNHqGzztFn8tsP3TpTRXYaWpnJOr6mfgx",
                 secret = "orYTkCQ5YdLfCjzTowOWmLnIiYdNGmkXx4RhMK6K",
                 redirect_uri = "http://localhost:1410/")

QBOtoken <- oauth2.0_token(endpoint = endPoint,
                           app = App,
                           scope = "com.intuit.quickbooks.accounting",
                           type = "code",
                           cache = T)

GET("https://sandbox-quickbooks.api.intuit.com/v3/company/123146073984434/query?query=select%20%2a%20from%20Customer&minorversion=4", config(token = QBOtoken))
