module TestData.BareKeywordLabel

// A bare upper-cased keyword on its own is a label / HTTP method, not a SQL
// statement. Typical shape: an HTTP verb held in a string constant.
let httpMethod = "DELETE"
let selectMode = "SELECT"
