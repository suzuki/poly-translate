# poly-translate Testing Automation

このドキュメントでは、poly-translateの自動化されたテスト環境について説明します。

## 概要

poly-translateでは、**修正→手動テスト→エラー発見**のフィードバックループを自動化するため、包括的なテスト環境を構築しました。

## ローカル開発でのテスト

### 利用可能なコマンド

```bash
# 全テストの実行
make test

# 個別テストの実行
make test-unit              # 基本単体テスト
make test-backend          # バックエンド固有テスト
make test-interactive      # 対話的コマンドテスト
make test-native-compile   # ネイティブコンパイルテスト

# コンパイル関連
make compile               # バイトコンパイル
make clean                 # コンパイル済みファイル削除
make check                 # 構文チェック

# その他
make install-deps          # 依存関係インストール
make run                   # poly-translateを読み込んでEmacs起動
make help                  # ヘルプ表示
```

### テスト結果の例

```
$ make test-unit
Running unit tests...
Running 15 tests (2025-06-28 19:33:58+0900, selector 't')
   passed  15/15  (1.103974 sec)

$ make test-backend
Running backend tests...
Running 13 tests (2025-06-28 19:34:09+0900, selector 't')
   passed  13/13  (0.296597 sec)
```

## CI/CD (GitHub Actions)

### 設定

- **プラットフォーム**: Ubuntu Runner (コスト効率重視)
- **Emacsバージョン**: 30.0 と snapshot (HEAD)
- **マトリックステスト**: 両バージョンで並行実行

### ワークフロー

1. **依存関係インストール**: gptel等の必要なパッケージ
2. **バイトコンパイル**: 全ファイルの構文チェック
3. **単体テスト実行**: ERTフレームワークを使用
4. **パッケージロード確認**: require文の動作確認
5. **文書チェック**: checkdocによる文書化確認

### トリガー

- `main`, `develop` ブランチへのpush
- `main` ブランチへのプルリクエスト

## テスト構成

### 1. 基本単体テスト (`tests/poly-translate-test.el`)

- エンジン登録・削除
- バックエンド管理
- キャッシュ機能
- レート制限
- 言語検出
- APIキー処理（関数型/文字列型）
- エラーハンドリング

### 2. バックエンド固有テスト (`tests/poly-translate-backend-test.el`)

- **DeepL**: API URL選択、言語マッピング、APIキー処理
- **Google Translate**: API定数、APIキー処理
- **LLM**: プロンプト生成、設定検証、各プロバイダー対応
- 統合テスト・エラーハンドリング

### 3. 対話的コマンドテスト (`tests/poly-translate-interactive-test.el`)

- `M-x poly-translate-region`
- `M-x poly-translate-buffer`
- `M-x poly-translate-string`
- バッファ表示・UI操作
- エンジン選択ロジック
- キーバインド確認

### 4. テストヘルパー (`tests/test-helper.el`)

- モック機能（HTTPリクエスト、バックエンド、エンジン）
- セットアップ・ティアダウン
- アサーション補助関数
- タイムアウト処理

## モック機能

### HTTP リクエストモック

```elisp
;; モック使用例
(poly-translate-test-with-http-mocking
  (poly-translate-test-mock-http-response
   "https://api-free.deepl.com/v2/translate"
   (poly-translate-test-deepl-success-response "こんにちは"))

  ;; 実際のAPI呼び出しなしでテスト実行
  (poly-translate-with-engine "DeepL" "Hello" ...))
```

### バックエンドモック

```elisp
;; モックバックエンド登録
(poly-translate-test-register-mock-backend 'mock-backend)
(poly-translate-test-register-mock-engine "Mock Engine" 'mock-backend "en" "ja")
```

## 実装された修正

### APIキー処理の統一

全バックエンド（DeepL、Google、LLM）で関数型APIキーをサポート：

```elisp
;; 文字列APIキー
(poly-translate-register-deepl-engine "DeepL" "auto" "ja" "your-api-key" nil)

;; 関数APIキー
(poly-translate-register-deepl-engine "DeepL" "auto" "ja" #'my-get-api-key nil)
```

### UI表示の修正

- "Translating..." プレースホルダーの正しい置換
- 複数エンジン結果の適切な表示
- バッファ内での正しい検索・置換処理

## 効果

### 開発効率向上

- **自動早期発見**: 構文エラー、未定義関数等を即座に検出
- **リグレッション防止**: 既存機能の破損を自動チェック
- **複数環境対応**: 異なるEmacsバージョンでの動作保証
- **包括的カバレッジ**: UI操作からバックエンド処理まで網羅

### 品質保証

- **実際の使用パターンテスト**: `M-x poly-translate-region` 等の実コマンド
- **エラー条件の確認**: 無効な設定、APIエラー等の処理
- **互換性検証**: 新機能追加時の既存機能への影響確認

## 今後の拡張

1. **実APIテスト**: 実際のTranslation APIを使用した統合テスト（オプション）
2. **パフォーマンステスト**: 大量テキスト翻訳時の性能測定
3. **UI自動化**: emacsクライアントでの操作自動化
4. **カバレッジ測定**: テストカバレッジの可視化

このテスト環境により、poly-translateの開発・保守が大幅に効率化され、高品質なパッケージの提供が可能になりました。