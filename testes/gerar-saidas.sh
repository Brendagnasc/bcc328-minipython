#!/usr/bin/env bash
# Gera os arquivos testes/saidas-esperadas.txt (TP1: ASTs e erros
# sintaticos) e testes/saidas-tp2.txt (TP2: execucao e erros de tipo),
# para conferencia manual e para o relatorio.
set -u
cd "$(dirname "$0")/.."

cabal build -v0 exe:mpyc || exit 1
mpyc=$(cabal list-bin mpyc)

saida="testes/saidas-esperadas.txt"
: > "$saida"

{
  echo "Saidas esperadas dos programas de teste"
  echo "Brenda Gabrielle Alves Nascimento, 24.1.4011"
  echo "BCC328 - Trabalho Pratico 1"
  echo
  echo "Programas validos: AST impressa com drawTree, identica nos tres analisadores."
  echo
} >> "$saida"

for f in mdc fib busca selecao alta_ordem formas pilha lambdas continuacao funcao_local; do
  {
    echo "===================================================================="
    echo "exemplos/$f.mpy  (--parse-rd, --parse-peg e --parse-lalr)"
    echo "===================================================================="
    "$mpyc" --parse-rd "exemplos/$f.mpy" 2>&1
  } >> "$saida"
done

{
  echo "===================================================================="
  echo "Programas invalidos: mensagem de erro de cada analisador"
  echo "(a mesma do arquivo .err ao lado do programa)"
  echo "===================================================================="
  echo
} >> "$saida"

for f in erro_char erro_string erro_separador erro_expoente erro_indent erro_sem_colon \
         erro_if_colon erro_alvo erro_rel erro_condicional erro_lambda_operando erro_break_local; do
  {
    echo "-- exemplos/$f.mpy"
    for modo in --parse-rd --parse-peg --parse-lalr; do
      printf '   %-13s %s\n' "$modo" "$("$mpyc" $modo "exemplos/$f.mpy" 2>&1 | head -1)"
    done
    echo
  } >> "$saida"
done

echo "gerado: $saida"

# Casos do TP2 (secao 5 do enunciado): codigo, entrada e saida
# esperada dos oito programas validos e dos programas com erro
# semantico, alem das mensagens dos demais testes de erro.
saida="testes/saidas-tp2.txt"
: > "$saida"

{
  echo "Casos de teste do verificador de tipos e do interpretador"
  echo "Brenda Gabrielle Alves Nascimento, 24.1.4011"
  echo "BCC328 - Trabalho Pratico 2"
  echo
  echo "Programas validos: saida de mpyc --run com a entrada mostrada."
  echo "Programas com erro: saida de erro de mpyc --check (igual ao .err)."
  echo
} >> "$saida"

caso() {
  local f=$1 modo=$2
  local entrada="${f%.mpy}.in"
  {
    echo "===================================================================="
    echo "$f  ($modo)"
    echo "===================================================================="
    cat "$f"
    if [ -f "$entrada" ]; then
      echo "---- entrada"
      cat "$entrada"
    else
      entrada=/dev/null
    fi
    echo "---- saida"
    "$mpyc" "$modo" "$f" < "$entrada" 2>&1
    echo
  } >> "$saida"
}

for f in secao_9_01_mdc secao_9_02_fibonacci secao_9_03_busca_linear secao_9_04_selecao \
         secao_9_08_alta_ordem secao_9_11_formas secao_9_12_pilha secao_9_15_lambdas; do
  caso "exemplos/especificacao/$f.mpy" --run
done

for f in e05_variavel_nao_declarada e16_int_mais_str e04_aridade_errada e03_funcao_sem_return \
         e17_break_fora_laco e18_campo_inexistente; do
  caso "exemplos/invalid/semantic/$f.mpy" --check
done

{
  echo "===================================================================="
  echo "Demais programas com erro de tipo (mpyc --check)"
  echo "===================================================================="
  echo
} >> "$saida"
for f in exemplos/invalid/semantic/*.mpy; do
  case "$(basename "$f" .mpy)" in
    e05_*|e16_*|e04_*|e03_*|e17_*|e18_*) continue ;;
  esac
  { echo "-- $f"; "$mpyc" --check "$f" 2>&1; } >> "$saida"
done

{
  echo "===================================================================="
  echo "Erros em tempo de execucao (mpyc --run)"
  echo "===================================================================="
  echo
} >> "$saida"
for f in exemplos/invalid/runtime/*.mpy; do
  entrada="${f%.mpy}.in"
  [ -f "$entrada" ] || entrada=/dev/null
  { echo "-- $f"; "$mpyc" --run "$f" < "$entrada" 2>&1; echo; } >> "$saida"
done

echo "gerado: $saida"
