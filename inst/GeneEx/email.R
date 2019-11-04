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
6866803080-iiestcu1e89kiv24foc9tp6l97ouhjmm.apps.googleusercontent.com
0wYqiYPH5XsMG-iLECEcyjeG
