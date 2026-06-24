module TestData.SqlInDocComment

// English UI strings that merely START with a SQL keyword (CREATE / DELETE /
// UPDATE) must not be flagged as raw SQL. Repro: thellma/intelligence I18n.fs.
type Translations =
    { SignUpSubtitle: string
      CreateAccount: string
      DeleteAccount: string
      UpdateTopic: string }

let private en: Translations =
    { SignUpSubtitle = "Create your account to start receiving personalized legal briefings"
      CreateAccount = "Create account"
      DeleteAccount = "Delete Account"
      UpdateTopic = "Update Topic" }
