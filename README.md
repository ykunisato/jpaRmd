# jpaRmd

<img src="inst/jpaRmd.png" align="right" alt="jpaRmd" width="180" />

<!-- badges: start -->
<!-- badges: end -->

jpaRmdは，日本心理学会の『心理学研究』への投稿用PDF原稿をRmarkdownで作成するためのテンプレートを提供するRパッケージです。[日本心理学会の「執筆・投稿の手びき(2015年版)」](https://psych.or.jp/manual/)に基づいています。また，認知行動療法学会の『認知行動療法』のテンプレートも用意しています。jpaRmdは既に基本的な開発は終了しており，後続するプロジェクトとして[jpaQmd](https://github.com/ykunisato/jpaQmd)がありますので，jpaQmdの使用をご検討ください。

## インストール

以下のコマンドをRコンソールに打ち込んで，Github経由でインストールしてください(remotesがない方はinstall.packages()でインストールください)。

``` r
# install.packages("remotes")
remotes::install_github("ykunisato/jpaRmd")
```

## 使用法

### 『心理学研究』

RStudioで，「File」->「New File」->「R Markdown…」をクリックして，「From Template」から「Japanese Psychological Association format{jpaRmd}」を選ぶ。「Name:」は，デフォルトではUntitledになっていますが，適宜変更してください。最後に，「OK」をクリックしてください。

Rmdファイルが自動的に開くと思います。上から７行目のRmd_file:が“Untitled.Rmd”のままになっているかと思います。これは，Rmdファイル名と同じにしないといけないので，変更します。これができたら，一度，「knit」をクリックしてください。

しばらくすると，フォルダ内にいくつかファイルが作られます。その中のPDFファイルを開くと，『心理学研究』に合ったフォーマットになっているかと思います。
『心理学研究』は25文字×32行で組みますが，改行にはご注意ください。jpaRmdの内部はTeXに変換するプロセスがありますが，TeXではRmd上の改行が無視されます。文を改行したい場合はバックスラッシュを一つ入れてください。改行後の字下げも自動的には行われませんので，全角スペースを入れる必要があります(半角スペースは改行記号同様，無視されます)。

### 『認知行動療法研究』

RStudioで，「File」->「New File」->「R Markdown…」をクリックする。以下の画面がでてきたら，「From Template」から「Japanese　Journal of Behavioral and Cognitive Therapies format{jpaRmd}」を選ぶ。「Name:」は，デフォルトではUntitledになっていますが，適宜変更ください。最後に「OK」をクリックしてください。Rmdファイルが自動的に開くと思います。

Rmdファイルが自動的に開くと思います。上から５行目のRmd_file:が“Untitled.Rmd”のままになっているかと思います。これは，Rmdファイル名と同じにしないといけないので，変更します。これができたら，一度，「knit」をクリックしてください。

しばらくすると，フォルダ内にいくつかファイルが作られます。PDFファイルを開くと，『認知行動療法研究』に合ったフォーマットになっているかと思います。

### 『心理学評論』

RStudioで，「File」->「New File」->「R Markdown…」をクリックする。以下の画面がでてきたら，「From Template」から「Japanese Psychological Review format{jpaRmd}」を選ぶ。「Name:」は，デフォルトではUntitledになっていますが，適宜変更ください。最後に「OK」をクリックしてください。Rmdファイルが自動的に開くと思います。

上から５行目のRmd_file:が“Untitled.Rmd”のままになっています。これは，Rmdファイル名と同じにしないといけないので，変更します。これができたら，一度，「knit」をクリックしてください。

しばらくすると，フォルダ内にいくつかファイルが作られます。PDFファイルを開くと，『心理学評論』に合ったフォーマットになっているかと思います（著者名なしと要約なし，著者名なし要約あり，著者名あり要約なし，著者名あり要約ありの４種類が出力される）。


### 引用文献についての注意

jpaRmdは，Bibファイルを使った引用が可能です。日本語文献の場合は，準備にいくつかルールがあります（文字コードはUTF-8，yomiを追加など）。[jpa\_citeに合ったBibファイルの作り方](https://qiita.com/kosugitti/items/63140ead7942d4e9b1d7)を参照いただいて，Bibファイルをご準備ください。

APAスタイル第7版に合わせて，初出であっても3名以上の共著を引用する際には"et al"を用いるように変更しました(Version 0.5.5から)。デフォルトでは3名以上の共著は初出から"et al"としますが，render_XXX関数においてuse.et.al.first = Fにすると初出の場合は３名以上でも"et al"としないようにもできます。

### Research Compendium

Research
Compendiumの関数も用意しています。以下のように，『心理学研究』用のset_rc_jpa()関数もしくは『認知行動療法研究』用のset_rc_jabct()関数を使って，引数にプロジェクト名をいれると（なおスペースは避けてください），Rmdファイルや解析，データを配置するフォルダなども準備されます。

-   『心理学研究』用Research Compendium

``` r
library(jpaRmd)
set_rc_jpa("rmarkdown_for_reproducibility")
``` 

-   『認知行動療法研究』用Research Compendium

``` r
library(jpaRmd)
set_rc_jabct("rmarkdown_for_reproducibility")
``` 


上記を実行すると“rmarkdown_for_reproducibility”という名前のディレクトリーの下に以下の下位ディレクトリーができます。paperディでクトリ内のpaper.Rmdを開いて，Knitを押してください。PDFが生成されます。

-   analysis: 分析用ファイルを置く場所
-   data：　データを置く場所
-   function：　分析で使う汎用関数を定義したりした場合にそのファイルを置く場所
-   materials：　研究で使ったマテリアルを置く場所
-   paper：　論文原稿を置く場所
-   README.md

### 査読コメントに対するリプライと修正対照表

査読コメントに対するリプライと修正対照表を作ることができます。RStudioで，「File」
-> 「New File」 ->「R Markdown…」
をクリックする。以下の画面がでてきたら，「From Template」から「Reply to
Reviewer{jpaRmd}」を選びます。「OK」をクリックしてください。


詳しい使い方は，[jpaRmdで再現可能な査読対応をしちゃおう！](https://cpp-laboratory.hatenablog.com/entry/2020/12/19/054240)を参照ください。

[本パッケージのウェブサイト](https://ykunisato.github.io/jpaRmd/)
