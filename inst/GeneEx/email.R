options(httr_oob_default = TRUE, httr_oauth_cache=TRUE)
gmailr::gm_deauth()
gmailr::gmail_auth()

options(httr_oauth_cache=T)
options(httr_oob_default=T)
gmailr::gmail_auth()

library(gmailr)
test_email <-
  gm_mime() %>%
  gm_to("vivek.kohar@jax.org") %>%
  gm_from("geneex.maintainer@gmail.com") %>%
  gm_subject("this is just a gmailr test") %>%
  gm_text_body("Can you hear me now?")

# Verify it looks correct
gm_create_draft(test_email)

# If all is good with your draft, then you can send it
gm_send_message(test_email)
gm_auth_configure(path = "~/Downloads/credentials.json")

Client ID
gm_auth_configure("6866803080-iiestcu1e89kiv24foc9tp6l97ouhjmm.apps.googleusercontent.com",
"0wYqiYPH5XsMG-iLECEcyjeG")
Steps to host Gmailr in Shiny Apps:
  
  1) set your options locally to be:
  
  options(httr_oob_default = TRUE, httr_oauth_cache=TRUE)

gmailr::clear_token()

Run your Shiny App.

2) A popup will appear for you to select your gmail account.

3) Instead of automatically connecting you, an authentication code will appear. You need to copy and paste that authentication code in your r console where it prompts you to paste it.

4) In your work directory, you will now have a httr oauth file saved. That is what grants you access to your gmail account.

5) Exit the shiny app. Remove the options from the script and save:
  
  options(httr_oob_default = TRUE, httr_oauth_cache=TRUE)

gmailr::clear_token()

6) Upload the shiny app with the httr oauth file in your directoy to shinyapps.io

App should call your gmail account now without asking you to authenticate!