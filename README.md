
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jpaRmd

<!-- badges: start -->

<!-- badges: end -->

jpaRmdは，日本心理学会の『心理学研究』への投稿用PDF原稿をRmarkdownで作成するためのパッケージです。

以下の日本心理学会の「執筆・投稿の手びき(2015年版)」に基づいています。引用文献などはまだ完璧とは言い難いですが，投稿用原稿にほぼ近いものができると思います。

<https://psych.or.jp/manual/>

## インストール

以下のコマンドをRコンソールに打ち込んで，Github経由でインストールしてください。

    # install.packages("remotes")
    remotes::install_github("ykunisato/jpaRmd")

## 使用法

  - RStudioで，「File」 -\> 「New File」 -\> 「R Markdown…」
    をクリックする。以下の画面がでてきたら，「From
    Template」から「Japanese Psychological Association
    format{jpaRmd}」を選んで，OKをクリックする。開かれた.RmdファイルをKnitください。

  - Research
    Compendiumの関数も用意していて，以下のようにset\_rc\_jpa()関数を使って，引数にプロジェクト名をいれると（なおスペースは避けてください），『心理学研究』用のRmdや解析，データを配置するフォルダなども準備されます。

<!-- end list -->

    library(jpaRmd)
    set_rc_jpa("rmarkdown_for_reproducibility")

上記を実行すると“rmarkdown\_for\_reproducibility”という名前のディレクトリーの下に以下の下位ディレクトリーができます。paperディでクトリ内のpaper.Rmdを開いて，Knitを押してください。PDFが生成されます。

  - analysis: 分析用ファイルを置く場所
  - data：　データを置く場所
  - function：　分析で使う汎用関数を定義したりした場合にそのファイルを置く場所
  - materials：　研究で使ったマテリアルを置く場所
  - paper：　論文原稿を置く場所
  - README.md

## 引用文献リストについて

引用文献については，このパッケージ内にあるjpa\_cite()関数を使うことで日本心理学会編「論文執筆投稿の手引き」にある書式と同じ書式で出力することができます。

### 準備するもの

  - 論文を書いているRmdファイル。例としてhogehoge.Rmdとします。
  - 引用文献リストの入ったbibファイル。例としてfuga.bibとします。

### 使い方

次の三つのステップで引用文献リストが入ります。

1.  ヘッダー部分を2箇所修正します。 1.　output: jpaRmd::jpa\_jjpとなっているところを，output:
    jpaRmd::jpa\_jjp2とします。 2.　次の一行を挿入します。suppress-bibliography: yes
2.  論文を書いている**Rmdファイルの中で**，次のようにjpa\_cite()関数を呼び出します。関数を呼び出す場所はどこでも結構です。

`library(jpaRmd); jpa_cite("hogehoge.Rmd","fuga.bib")`

3.  Rmdファイルの中で，引用文献リストを挿入したい場所に

`\input{fugafuga}`

と書きます。
