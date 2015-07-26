#' Send an email with template
#'
#' This sends an email with template
#'
#' @param api_key your Mandrill API Key.
#'
#' @param template_name name of template on Mandrill to use
#' @param global_variables list of global merge variables used in template to be filled in.
#' Mandrill expects these to come as an array, with each key/value pair split into
#' name: key, content: value.  for instance, if your key was named 'orderid', and the value was
#' 'order1019', Mandrill would expect the JSON for these merge variables to look like 
#' 
#' \code{`global_merge_vars": [{"name": "orderid", "content": "order1019"}]`}.  
#' 
#' one easy way to pass these variables is as a data frame.  using our example above,
#' 
#' \code{global_variables = data.frame('name'=c('orderid'), 'content'=c('order1019'))} 
#' 
#' will be converted by jsonlite::toJSON into an JSON array that Mandrill can understand.
#' @param subject character vector (single element) to populate email subject line
#' @param recipient character vector of email addresses of recipients, or data.frame with
#' headers "email", "name", and "type", per mandrill documentation.
#' @param sender character vector of senders' email addresses
#' @param contents list HTML contents to be replaced in Mandrill template.  These are used to replace content
#' between HTML tags that have a CSS class of `mc:edit="content_name"` where the name of the object in the list
#' must match the the `content_name` in in the template. 
#' @param images character vector denoting location PNGs to include in email. PNG file names (minus the suffix and 
#' any preceeding path directories) must match the image declaration in the Mandrill template. 
#' @param css whether to include inline CSS or not.  This CSS must be already present in the Mandrill template 
#'
#' @return contents of HTTP Response.
#'
#' @references \url{https://mandrillapp.com/api/docs/messages.JSON.html#method=send} has examples
#' of valid JSON for sending a message using Mandrill.
#' 
#' @export

mandrill_send_template <- function(api_key = NA, 
                                   template_name = NA, 
                                   global_variables = NA, 
                                   subject = NA, 
                                   recipient = NA, 
                                   sender = NA,
                                   contents = NULL,
                                   images = NULL,
                                   css = FALSE) {
  if (class(recipient) == "data.frame") {
    to <- recipient
  } else {
    to <- data.frame(email = recipient, stringsAsFactors = FALSE)
  }

  images_out <- NA
  if (!missing(images)) {    
    images_out <- dplyr::rbind_all(lapply(images, process_images))
  }

  template_contents <- data.frame()
  if (!missing(contents)) {
    for(content in names(contents)) {
      content_name <- content
      out_df <- data.frame(name = content_name,
                           content = contents[[content]])
      template_contents <- rbind(template_contents, out_df)
    }
  }
  
  
  sendData <- list(key = api_key, 
                   template_name = template_name, 
                   template_content = template_contents, 
                   message = list(subject = subject, 
                                  global_merge_vars = global_variables, 
                                  to = to, 
                                  from_email = sender,
                                  images = images_out,
                                  inline_css = css
                                  )
                   )
  link <- "https://mandrillapp.com/api/1.0/messages/send-template.json"
  jsonData <- jsonlite::toJSON(sendData, auto_unbox = TRUE)
  print(jsonData)
  .post(link, jsonData)
}

.post <- function(link, data) {
  res <- httr::POST(url = link, httr::user_agent(agent = "Mandrill-Curl/1.0"), body = data)
  content(res)
}
