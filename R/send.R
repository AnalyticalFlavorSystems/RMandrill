#' Send an email with template
#'
#' This sends an email with template
#'
#' @param api_key Your Stripe API Key
#'
#' @param template_name name of template on Mandrill to use
#' @param variables list of variables used in template to be filled in
#' @param subject character vector (single element) to populate email subject line
#' @param recipient character vector of email addresses of recipients
#' @param sender characer vector of sendes' email addresses
#' @param contents  contents of email
#' @param images character vector of PNGs to include in email.  names (minus the suffix)
#' must match the image declaration in the Mandrill template. Also note that images must 
#' saved in same directory as calling environment.
#' @param css whether to include inline CSS or not. 
#'
#' @return nothing
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
  to <- data.frame(email=recipient, stringsAsFactors = FALSE)
  merge_vars <- data.frame(rcpt=recipient, stringsAsFactors = FALSE)
  merge_vars$vars <- list(variables)

  images_out<-NA
  if(!missing(images)){    
    process_images <- function(image){
      # create tempfile
      temp<-tempfile()
      
      #get image name
      image_name<-gsub("(.+\\/)(.+$)", "\\2", image, perl = T) %>% 
        stringr::str_replace("(//|/)", "") %>% 
        stringr::str_replace(".png", "")
        
      
      #enocde png as base64
      base64::encode(image, temp) # called for side effect.  saves encoded image in temp
      
      # get encoded image
      image_encoded<-paste(readLines(temp), collapse="")
      
      #unlink temp file
      unlink(temp)
      
      out_df<-data.frame(type="img/png", 
                         name=image_name, 
                         content=image_encoded,
                         stringsAsFactors = FALSE
                         )
      
      
      return(out_df)
    }
    
    images_out<-dplyr::rbind_all(lapply(images, process_images))
    
  }

  template_contents<-data.frame()
  if(!missing(contents)){
    
    process_contents<-function(content){
      content_name<-deparse(substitute(content))
      out_df<-data.frame(name=content_name,
                         content=content[[1]],
                         stringsAsFactors = FALSE)
      
      return(out_df)
    }
    
    #template_contents<-lapply(contents,function(x) process_contents(x))
    for(content in names(contents)){
      content_name<-content
      out_df<-data.frame(name=content_name,
                         content=contents[[content]])
      template_contents<-rbind(template_contents,out_df)
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
