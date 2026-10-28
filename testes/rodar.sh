#!/usr/bin/env bash
# Suite de testes do analisador lexico e sintatico do MiniPython.
#
# Programa com arquivo .err ao lado e invalido: a saida de erro de cada
# analisador tem que ser exatamente o conteudo do .err, como na secao 7
# do enunciado. Os demais sao validos e os tres analisadores tem que
# imprimir a mesma AST. Os de exemplos/invalid/semantic so tem erros de
# tipo, que sao do proximo trabalho, e aqui contam como validos.
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
}

for src in $(find exemplos -name '*.mpy' | sort); do
  err="${src%.mpy}.err"
  if [ -f "$err" ] && [[ "$src" != exemplos/invalid/semantic/* ]]; then
    # Sem opcao o padrao e --parse-rd; testado tambem, porque e assim
    # que o programa costuma ser chamado.
    certo=1
    for modo in "" --parse-rd --parse-peg --parse-lalr; do
      if "$mpyc" $modo "$src" > /dev/null 2> "$tmp/err" || ! cmp -s "$tmp/err" "$err"; then
        falhou "$src ${modo:-(sem opcao)}"
        certo=0
      fi
    done
    [ $certo -eq 1 ] && ok=$((ok + 1))
  else
    "$mpyc" --parse-rd   "$src" > "$tmp/rd"   2>&1 &&
    "$mpyc" --parse-peg  "$src" > "$tmp/peg"  2>&1 &&
    "$mpyc" --parse-lalr "$src" > "$tmp/lalr" 2>&1 &&
    cmp -s "$tmp/rd" "$tmp/peg" && cmp -s "$tmp/rd" "$tmp/lalr"
    if [ $? -eq 0 ]; then ok=$((ok + 1)); else falhou "$src"; fi
  fi
done

rm -rf "$tmp"
echo "Total: $ok ok, $falhas falhas"
[ $falhas -eq 0 ]
