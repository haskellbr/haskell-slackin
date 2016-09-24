# haskell-slackin
Uma re-implementação parcial da aplicação de convites do Slack `slackin` em
Haskell, para servir como um exemplo de um WebApp não trivial em Haskell.

O que você pode esperar aprender aqui, no momento (tenho que limpar os branches
para fazer um tutorial mais claro):

## `1-initial-steps`
- Configurando o projeto
- Adicionando o Yesod, sem nenhum boilerplate
- Servindo HTML com o Yesod e Hamlet
- Servindo formulários usando a geração de forms do Yesod
- Capturando os dados do formulário e servindo uma página de confirmação

## `2-slack-invite`
- Enviando um invite do Slack para os e-mails capturados pelo formulário por
  meio da sua API HTTP
- Ler a configuração (organização e token do Slack) das variáveis do ambiente e
  adicionar esses dados ao estado da aplicação

## `3-slack-presence`
- Adicionar threads de background para nossa aplicação
- Compartilhar o estado entre essas threads e implementar publish-subscribe
  usando STM e channels
- Adicionar JavaScript às páginas enviadas pelos `Handler`s do Yesod
- Usar a implementação de publish-subscribe para publicar eventos em um handler
  de WebSockets
- Receber esses eventos no cliente
- Gerar classes para serializar o estado do Slack em JSON
- Ler o estado compartilhado e o enviar no primeiro render dos templates

- - -

## Licença
```
The MIT License (MIT)

Copyright (c) 2016 Pedro Tacla Yamada

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
```
