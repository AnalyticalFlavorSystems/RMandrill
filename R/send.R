#' Send an email with template
#'
#' This sends an email with template
#'
#' @param api_key Your Stripe API Key
#'
#' @param template_name name of template on Mandrill to use
#' @param variables list of merge variables used in template to be filled in
#' @param subject character vector (single element) to populate email subject line
#' @param recipient character vector of email addresses of recipients
#' @param sender character vector of senders' email addresses
#' @param contents list HTML  contents to be replaced in Mandrill template.  These are used to replace content
#' between HTML tags that have a CSS class of `mc:edit="content_name"` where the name of the object in the list
#' must match the the `content_name` in in the template. 
#' @param images character vector denoting location PNGs to include in email. PNG file names (minus the suffix and 
#' any preceeding path directories) must match the image declaration in the Mandrill template. 
#' @param css whether to include inline CSS or not.  This CSS must be already present in the Mandrill template 
#'
#' @return contents of HTTP Response.
#'
#' @export
#'
mandrill_send_template <- function(api_key=NA, 
                                   template_name=NA, 
                                   variables=NA, 
                                   subject=NA, 
                                   recipient=NA, 
                                   sender=NA,
                                   contents=NULL,
                                   images=NULL,
                                   css=FALSE) {
  to <- data.frame(email=recipient, 
                   stringsAsFactors = FALSE)
  merge_vars <- data.frame(rcpt=recipient, 
                           stringsAsFactors = FALSE)
  merge_vars$vars <- list(variables)

  images_out <- NA
  if(!missing(images)) {    
    images_out <- dplyr::rbind_all(lapply(images, process_images))
  }

  template_contents <- data.frame()
  if(!missing(contents)) {
    for(content in names(contents)) {
      content_name <- content
      out_df <- data.frame(name=content_name,
                           content=contents[[content]])
      template_contents <- rbind(template_contents,out_df)
    }
  }
  
  
  sendData <- list(key=api_key, 
                   template_name=template_name, 
                   template_content=template_contents, 
                   message=list(subject=subject, 
                                merge_vars=merge_vars, 
                                to=to, 
                                from_email=sender,
                                images=images_out,
                                inline_css=css
                                )
                   )
  link <- "https://mandrillapp.com/api/1.0/messages/send-template.json"
  jsonData <- jsonlite::toJSON(sendData, auto_unbox=TRUE)
  .post(link, jsonData)
}

.post <- function(link, data) {
  res <- httr::POST(url = link, httr::user_agent(agent = "Mandrill-Curl/1.0"), body=data)
  content(res)
}
