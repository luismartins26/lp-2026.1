# 📚 CIC0093 - Linguagens de Programação (2026/1)

Bem-vindo ao repositório de materiais de estudo para a disciplina **CIC0093 - LINGUAGENS DE PROGRAMAÇÃO** do semestre 2026/1.

O objetivo deste projeto é centralizar exercícios práticos e materiais de apoio para facilitar o aprendizado dos paradigmas de programação abordados no curso.

## 🎯 Proposta de Estudo

A ideia central deste repositório é a **verificação imediata**. Para cada tópico, você encontrará exercícios que já vêm acompanhados de uma bateria de testes unitários. Isso permite que você saiba na hora se a sua lógica está correta.

### Como o conteúdo está organizado:
Cada pasta no repositório corresponde a um tópico específico da disciplina:

```text
/nome-do-topico
├── Exercicios.hs      # Arquivo com enunciados, tipos e TESTES (Comece aqui!)
└── Solucoes.hs        # Versão com as implementações sugeridas para conferência.
```

## 🛠️ Como Utilizar (Fluxo de Aprendizado)

Para aproveitar ao máximo o material, recomenda-se o seguinte fluxo:

1.  **Escolha um tópico:** Entre na pasta do assunto que deseja praticar (ex: `/Listas`).
2.  **Mão na massa:** Abra o arquivo de exercícios. As funções estarão marcadas como `undefined`.
3.  **Implemente e Teste:** Escreva sua solução e rode o arquivo no terminal.
4.  **O Desafio:** **Tente resolver todos os testes do arquivo de exercícios por conta própria antes de abrir o arquivo de soluções.** O erro faz parte do aprendizado!
5.  **Confira:** Após terminar (ou se ficar muito travado), consulte o arquivo de soluções para comparar diferentes abordagens.

## 🚀 Executando os Exercícios

Não é necessário configurar ambientes complexos como Docker. Você só precisa ter o compilador da linguagem instalado.

### Haskell
Certifique-se de ter o [GHCup](https://www.haskell.org/ghcup/) instalado. Para rodar os testes e ver os resultados:

```bash
runhaskell Exercicios.hs
```

## 🤝 Contribuições
Este repositório é feito para a turma! Se você encontrou um erro em algum teste, tem uma solução mais elegante para um problema ou quer adicionar novos exercícios:
1. Faça um Fork do projeto.
2. Crie uma branch para sua alteração.
3. Envie um Pull Request.

## ⚖️ Licença
Este projeto está sob a licença **MIT** - veja o arquivo `LICENSE` para detalhes. Sinta-se livre para usar o código para seus estudos e projetos pessoais.

---
*Bons estudos e que o compilador esteja com você!*