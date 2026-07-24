# stack buil および stack exec のラッパースクリプト

1. executables にエントリー追加
    ```console
    $ scripts4stack/setting hoge
    ```
    - `package.yaml`のexecutablesセクションに`hoge`のエントリーを追加。
    - `app/hoge/Main.hs`（ダミーファイル）を追加。
    - スクリプト`./.curname`でCURRENT_STACK_EXE_ENTRYをhogeに設定するようにする。
    - 仮ビルド
    - gen-hieがローカルにインストールされていれば、それを使って hie.yaml を更新。
2. ビルド
    ```console
    $ scripts4stack/building
    ```
    CURRENT_STACK_EXE_ENTRYをビルド
3. 実行
    ```console
    $ scripts4stack/executing
    ```
    CURRENT_STACK_EXE_ENTRYを実行
4. 再設定
    ```console
    $ scripts4stack/resetting huga
    ```
    CURRENT_STACK_EXE_ENTRYをhugaに設定（変数のみ変更）
    
