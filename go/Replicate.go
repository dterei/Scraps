package main
import "fmt"
import "strings"
func main() { fmt.Printf("%s\nvar s = \"%s\"\n", s, strings.Replace(strings.Replace(strings.Replace(s, "\\", "\\\\", -1), "\n", "\\n", -1), "\"", "\\\"", -1)) }
var s = "package main\nimport \"fmt\"\nimport \"strings\"\nfunc main() { fmt.Printf(\"%s\\nvar s = \\\"%s\\\"\\n\", s, strings.Replace(strings.Replace(strings.Replace(s, \"\\\\\", \"\\\\\\\\\", -1), \"\\n\", \"\\\\n\", -1), \"\\\"\", \"\\\\\\\"\", -1)) }"
