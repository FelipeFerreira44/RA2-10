# RA2-10


## Informações do Projeto

**Instituição:** Pontifícia Universidade Católica do Paraná (PUCPR)  
**Disciplina:** Programação Funcional  
**Professor:** Frank Coelho de Alcantara  
**Atividade:**  RA2 - Funcional Inventário em Haskell

### Grupo 

| Nome do Aluno | GitHub Username | Responsabilidade |
|---------------|-----------------|------------------|
| **ANDRE VINICIUS MARTINS DE SOUZA ACOSTA DE JESUS** | @Andrekz | Definição de Tipos, Lógica de Negócio, Análise de Dados e Parsing de Comandos |
| **Felipe Willian Barros Ferreira** | @FelipeFerreira44 | Interface Principal, Persistência de Dados, Documentação e Testes |

---

## Descrição do Sistema

Sistema de gerenciamento de inventário desenvolvido em Haskell que implementa:

-  Separação clara entre lógica pura e operações de I/O
-  Persistência de dados em arquivo (`Inventario.dat`)
-  Log de auditoria (`Auditoria.log`)
-  Tratamento de erros com `Either`
-  Validação de estoque e operações
- Interface interativa via terminal

---
### 📁 Arquivos de Persistência
- **`Inventario.dat`** - Estado atual do inventário (sobrescrito)
- **`Auditoria.log`** - Histórico completo de operações (append-only)

---

## 🏗️ Arquitetura do Sistema
```
projeto/
├── InventarioTipos.hs # Definições de tipos e estruturas
├── Logica.hs # Funções puras de negócio
├── ParserComandos.hs # Interpretação de comandos
├── Analise.hs # Análise e relatórios
├── Main.hs # Loop principal e I/O
├── Inventario.dat # Dados persistidos (automático)
├── Auditoria.log # Logs de auditoria (automático)
└── README.md # Documentação
```
 ---

## Execução no OnlineGDB

1.	Acesse: https://onlinegdb.com/YK587TaJp
2. Selecione linguagem: **Haskell**
3. Copie todos os arquivos `.hs` para o editor (caso ainda não estejam todos lá)
4. Certifique-se de que `Main.hs` está como arquivo principal
5. Clique em **Run**

---

##  Comandos Disponíveis

### Adicionar Item
```
add ID nome quantidade categoria
```
**Exemplo:** `add 001 Teclado 10 Perifericos`

### Remover Item
```
remove ID quantidade
```
**Exemplo:** `remove 001 5`

### Atualizar Quantidade
```
update ID nova_quantidade
```
**Exemplo:** `update 001 20`

### Buscar itens
```
search TERMO  
```

### Listar Inventário
```
list
```

### Relatório de logs
```
report
```

### Mostra os comandos
```
help
```

### Sair
```
exit
```

---

## Cenários de Teste

### Cenário 1: Persistência de Estado (Sucesso)

**Objetivo:** Verificar se o estado do inventário é persistido corretamente entre execuções.

**Passos executados:**
1. Iniciar programa sem arquivos de dados
2. Executar os comandos:
   ```
    add 11 camisa 11 roupas
    add 12 shorts 12 roupas  
    add 13 moletom 13 roupas
   ```
3. Sair com `exit`
4. Verificar se os arquivos `Inventario.dat` e `Auditoria.log` foram criados
5. Reiniciar o programa
6. Executar `list`

**Resultado:**
```
[1 of 6] Compiling InventarioTipos  ( InventarioTipos.hs, InventarioTipos.o )
[2 of 6] Compiling Analise          ( Analise.hs, Analise.o )
[3 of 6] Compiling Logica           ( Logica.hs, Logica.o )
[4 of 6] Compiling ParserComandos   ( ParserComandos.hs, ParserComandos.o )
[5 of 6] Compiling Main             ( main.hs, main.o )
[6 of 6] Linking a.out
=== SISTEMA DE GERENCIAMENTO DE INVENTÁRIO ===
Itens carregados: 0
Logs carregados: 0
Populando com dados iniciais (10 itens)...
Log registrado em Auditoria.log
...
Inventário salvo em Inventario.dat

> add 11 camisa 11 roupas
Inventário salvo em Inventario.dat
Log registrado em Auditoria.log
Item adicionado com sucesso!
> add 12 shorts 12 roupas
Inventário salvo em Inventario.dat
Log registrado em Auditoria.log
Item adicionado com sucesso!
> add 13 moletom 13 roupas
Inventário salvo em Inventario.dat
Log registrado em Auditoria.log
Item adicionado com sucesso!
> exit

=== SISTEMA DE GERENCIAMENTO DE INVENTÁRIO ===
Itens carregados: 13
Logs carregados: 13

> list
=== INVENTÁRIO ATUAL ===
001 | Teclado Mecânico | Qtd: 50 | Cat: Periféricos
002 | Mouse Óptico | Qtd: 75 | Cat: Periféricos
...
11 | camisa | Qtd: 11 | Cat: roupas
12 | shorts | Qtd: 12 | Cat: roupas  
13 | moletom | Qtd: 13 | Cat: roupas
Total de itens: 13
Total em estoque: 406 unidades
```

**Status:** Concluído com sucesso

---

### Cenário 2: Erro de Lógica (Estoque Insuficiente)

**Objetivo:** Verificar tratamento de tentativas de remoção inválidas.

**Passos executados:**
1. Adicionar um item com 10 unidades:
   ```
   add 14 teclado 10 tecnologia
   ```
2. Tentar remover 15 unidades:
   ```
   remove 14 15
   ```
3. Verificar mensagem de erro
4. Executar `list` para confirmar que ainda há 10 unidades
5. Verificar o arquivo `Auditoria.log`

**Resultado:**
```
> add 14 teclado 10 tecnologia
Inventário salvo em Inventario.dat
Log registrado em Auditoria.log
Item adicionado com sucesso!
> remove 14 15
Log registrado em Auditoria.log
Erro: Estoque insuficiente

LogEntry {..., acao = Remove, detalhes = "Estoque insuficiente", status = Falha "Estoque insuficiente"}

```

**Status:** Concluído com sucesso

---

### Cenário 3: Geração de Relatório de Erros

**Objetivo:** Verificar análise de logs e geração de relatórios.

**Passos executados:**
1. Após Cenário 2, executar::
   ```
   report
   ```
2. Verificar exibição do erro anterior 


**Resultado:**
```
=== RELATÓRIO DE ANÁLISE ===
Total de logs: 15
Logs de erro: 1
Taxa de sucesso: 93%
Item mais movimentado: Nenhum item movimentado

Último logs de erro:
22:38:48 [ERRO] Estoque insuficiente
```

**Status:** Concluído com sucesso
