module TestData.BareKeywordLabel

// A bare upper-cased keyword on its own is a label / HTTP method, not a SQL
// statement. Repro: data-action-method="DELETE" in intelligence admin views.
let httpMethod = "DELETE"
let selectMode = "SELECT"
