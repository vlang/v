# SMTP TLS 修正

## 修正方針

TLS の証明書チェーンとサーバー名を認証前に検証します。
検証失敗時は接続を閉じます。
TLS を使わない認証も既定で拒否し、明示設定がある場合だけ許可します。

## 変更内容

- `Config` に既定有効の `validate` と CA バンドル設定を追加しました。
- 暗黙 TLS と STARTTLS の両方で同じ検証設定とサーバー名を使います。
- `validate: false` は証明書チェーンとホスト名の検証を無効化します。
- `allow_insecure_auth: true` は平文認証だけを許可します。
- TLS 検証失敗時は AUTH 前に TCP 接続を閉じます。
- AUTH が拒否された場合も TCP 接続を閉じます。
- SMTP の例を更新し、ローカル TLS 回帰テストを追加しました。

## 検証

Nix 開発環境で SMTP テスト 4 件が両バックエンドで成功しました。

```sh
nix develop /Users/uaaw/src/vlang --command sh -c '
cd /Users/uaaw/src/vlang/v-ghsa-h7v5-7pwq-p3j3
./v -silent test vlib/net/smtp/
./v -d use_openssl -silent test vlib/net/smtp/
'
```
