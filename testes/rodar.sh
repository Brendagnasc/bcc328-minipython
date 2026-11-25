#!/usr/bin/env bash
# Suite de testes do MiniPython chamando o executavel, com as mesmas
# regras do cabal test (ver test/Main.hs):
#
# - .err em exemplos/invalid/semantic: a saida de mpyc --check tem que
#   ser exatamente o conteudo do .err;
# - .err em exemplos/invalid/runtime: mpyc --run termina com o erro do
#   .err;
# - .err nos demais diretorios: erro lexico ou sintatico, e a saida de
#   erro de cada analisador tem que ser o .err, como no TP1;
# - sem .err: os tres analisadores imprimem a mesma AST e o programa
#   passa no verificador;
# - .out: saida esperada de mpyc --run, com a entrada do .in, se houver.
#
# Uso: bash testes/rodar.sh

set -u
cd "$(dirname "$0")/.."

cabal build -v0 exe:mpyc || exit 1
mpyc=$(cabal list-bin mpyc)

ok=0
falhas=0
tmp=$(mktemp -d)

falhou() {
  echo "  FALHOU   $1"
  falhas=$((falhas + 1))
  certo=0
}

for src in $(find exemplos -name '*.mpy' | sort); do
  err="${src%.mpy}.err"
  out="${src%.mpy}.out"
  inp="${src%.mpy}.in"
  [ -f "$inp" ] || inp=/dev/null
  case "$src" in
    exemplos/invalid/semantic/*) tipo=semantico ;;
    exemplos/invalid/runtime/*)  tipo=execucao ;;
    *) if [ -f "$err" ]; then tipo=sintatico; else tipo=valido; fi ;;
  esac
  certo=1

  if [ $tipo = sintatico ]; then
    # Sem opcao o padrao e --parse-rd; testado tambem, porque e assim
    # que o programa costuma ser chamado.
    for modo in "" --parse-rd --parse-peg --parse-lalr; do
      if "$mpyc" $modo "$src" > /dev/null 2> "$tmp/err" || ! cmp -s "$tmp/err" "$err"; then
        falhou "$src ${modo:-(sem opcao)}"
      fi
    done
  else
    "$mpyc" --parse-rd   "$src" > "$tmp/rd"   2>&1
    "$mpyc" --parse-peg  "$src" > "$tmp/peg"  2>&1
    "$mpyc" --parse-lalr "$src" > "$tmp/lalr" 2>&1
    if ! cmp -s "$tmp/rd" "$tmp/peg" || ! cmp -s "$tmp/rd" "$tmp/lalr"; then
      falhou "$src (os tres analisadores diferem)"
    fi
    if [ $tipo = semantico ]; then
      if "$mpyc" --check "$src" 2> "$tmp/err" || ! cmp -s "$tmp/err" "$err"; then
        falhou "$src --check"
      fi
    elif ! "$mpyc" --check "$src" 2> /dev/null; then
      falhou "$src --check"
    elif [ -f "$out" ] || [ $tipo = execucao ]; then
      "$mpyc" --run "$src" < "$inp" > "$tmp/out" 2> "$tmp/err"
      status=$?
      if [ $tipo = execucao ]; then
        [ $status -ne 0 ] && cmp -s "$tmp/err" "$err" || falhou "$src --run (erro)"
      else
        [ $status -eq 0 ] && [ ! -s "$tmp/err" ] || falhou "$src --run (erro)"
      fi
      if [ -f "$out" ] && ! cmp -s "$tmp/out" "$out"; then
        falhou "$src --run (saida)"
      fi
    fi
  fi
  [ $certo -eq 1 ] && ok=$((ok + 1))
done

rm -rf "$tmp"
echo "Total: $ok ok, $falhas falhas"
[ $falhas -eq 0 ]
