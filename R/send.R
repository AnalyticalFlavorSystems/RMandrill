require(jsonlite)
require(httr)

#' Send an email with template
#'
#' This sends an email with template
#'
#' @param api_key Your Stripe API Key
#'
#' @param args An optional list which can contain an amount to refund
#'
#' @return nothing
#'
#' @export
#'
mandrill_send_template <- function(api_key, template_name, variables, subject, recipient, sender) {
  to <- data.frame(email=recipient)
  merge_vars <- data.frame(rcpt=recipient)
  merge_vars$vars <- list(variables)
  sendData <- list(key=api_key, template_name=template_name, 
                   template_content=list(), 
                   message=list(subject=subject, merge_vars=merge_vars, 
                                to=to, from_email=sender))
  link <- "https://mandrillapp.com/api/1.0/messages/send-template.json"
  jsonData <- toJSON(sendData, auto_unbox=TRUE)
  .post(link, jsonData)
}

.post <- function(link, data) {
    res <- POST(url = link, user_agent("Mandrill-Curl/1.0"), body=data)
    content(res)
}
