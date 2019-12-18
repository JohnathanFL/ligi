
import lexer
import node

when isMainModule:
  echo "Hello, World!"
  var a: Lexer
  discard a.scan()
