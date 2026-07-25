# scripts4stack の使い方

個別のソリューションは、とりあえず、以下の手順で作ることもできます。

`<problem-name>`は、`preblems`以下にあるファイルのベース名と同じにするとよいと思う。

1. executables にエントリー追加
    ```console
    $ scripts4stack/setting <problem-name>
    ```
    - `package.yaml`のexecutablesセクションに`<problem-name>`のエントリーを追加。
    - `app/<problem-name>/Main.hs`（ダミーファイル）を追加。
    - スクリプト`./.curname`でCURRENT_STACK_EXE_ENTRYを`<problem-name>`に設定。
    - 仮ビルド
    - gen-hieがローカルにインストールされていれば、それを使って hie.yaml を更新。
2. `solutions/txt/<problem-name>.txt`作成
3. `app/<problem-name>/Main.hs`作成
4. ビルド
    ```console
    $ scripts4stack/building
    ```
    CURRENT_STACK_EXE_ENTRYをビルド
5. 実行
    ```console
    $ scripts4stack/executing
    ```
    - CURRENT_STACK_EXE_ENTRYを実行する.
    - solutions/man/CURRENT_STACK_EXE_ENTRY.manができる。
6. 再設定
    ```console
    $ scripts4stack/resetting <problem-name>
    ```
    CURRENT_STACK_EXE_ENTRYをhugaに設定（変数のみ変更）
    
