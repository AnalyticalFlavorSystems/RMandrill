#' Encode PNG to base64 and provide meta data for JSON payload
#'
#'
#' @param image character vector of unencoded PNG file location
#'
#' @return data frame with image type, image name (which matches CID in Mandrill template), and
#' base 64 encoded image. 
process_images <- function(image){
  # create tempfile
  temp<-tempfile()
  
  #get image name
  image_name <- stringr::str_extract(image, "(/.+$)") %>% 
    stringr::str_replace("(//|/)", "") %>% 
    stringr::str_replace(".png", "")
  
  #enocde png as base64
  base64::encode(image, temp) # called for side effect.  saves encoded image in temp
  
  # get encoded image
  image_encoded<-paste(readLines(temp), collapse="")
  
  #unlink temp file
  unlink(temp)
  
  out_df <- data.frame(type="img/png", 
                     name=image_name, 
                     content=image_encoded
                     )

  # return
  out_df
}

#' Process HTML contents passed to `mc:edit` classes in Mandrill template
#' 
#' @param character vector of HTML to replace contents btween HTML tags in Mandrill Template
#'
#' @return data frame with content name (matching `mc:edit="content_name"` in template)
#' 
process_contents <- function(content){
  content_name<-deparse(substitute(content))
  out_df<-data.frame(name=content_name,
                     content=content[[1]])
  
  # return
  out_df
}
