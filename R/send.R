mandrill_send_template <- function(api_key=NA, 
                                   template_name=NA, 
                                   variables=NA, 
                                   subject=NA, 
                                   recipient=NA, 
                                   sender=NA,
                                   images=NULL) {
  to <- data.frame(email=recipient)
  merge_vars <- data.frame(rcpt=recipient)
  merge_vars$vars <- list(variables)

  images_out<-NA
  if(!missing(images)){    
    process_images <- function(image){
      # create tempfile
      temp<-tempfile()
      
      #get image name
      image_name<-str_extract(image, "(//.+$)") %>% 
        str_replace("(//|/)", "") %>% 
        str_replace(".png", "")
        
      
      #enocde png as base64
      encode(image, temp) # called for side effect.  saves encoded image in temp
      
      # get encoded image
      image_encoded<-paste(readLines(temp), collapse="")
      
      #unlink temp file
      unlink(temp)
      
      out_df<-data.frame(type="img/png", 
                         name=image_name, 
                         content=image_encoded
                         )
      
      
      return(out_df)
    }
    
    images_out<-dplyr::rbind_all(lapply(images, process_images))
    
  }
  
  
  sendData <- list(key=api_key, template_name=template_name, 
                   template_content=list(), 
                   message=list(subject=subject, 
                                merge_vars=merge_vars, 
                                to=to, 
                                from_email=sender,
                                images=images_out
                                )
                   )
  link <- "https://mandrillapp.com/api/1.0/messages/send-template.json"
  jsonData <- toJSON(sendData, auto_unbox=TRUE)
  .post(link, jsonData)
}

.post <- function(link, data) {
  res <- POST(url = link, user_agent("Mandrill-Curl/1.0"), body=data)
  content(res)
}