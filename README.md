# woorl

_woorl_ stands for _woody curl_.

Запускайте без аргументов, чтобы получить информацию, достаточную для того, чтобы начать пользоваться.

## Примеры использования

* Обработка и представление исключений:

        $ woorl -s ~/ws/damsel/proto/payment_processing.thrift \
            'http://localhost:8042/v1/processing/invoicing' \
            Invoicing Get '{"id":"42"}' '"0.834.0"'
        {
          "exception": "MachineNotFound",
          "data": {}
        }

        $ echo $?
        1

* Представление как получаемых от сервера, так и отправляемых клиентом нечитаемых binary в виде content object с кодированием в base64:

        $ woorl -s ~/ws/damsel/proto/cds.thrift 'http://cds:8022/v1/keyring' Keyring Init 2 3
        [
          {
            "content_type": "base64",
            "content": "AgHFMYEHOR1oGmRwPg20O/yyN2nwViIqTT+qtlNzGk/jig=="
          },
          {
            "content_type": "base64",
            "content": "AgKe7/slQul68mTtpOhjIlE45/NImIe94yWDYMVjouEjig=="
          },
          {
            "content_type": "base64",
            "content": "AgNcpSY7a050qmRt0kAu3sG1XIUgKeQ7cthv2beYynBjig=="
          }
        ]

## Известные проблемы

* По умолчанию базовой директорией для временных файлов (например, артефактов компиляции thrift файлов) является переменная среды `TMPDIR`, которая обычно недоступна, если вызов _thrift compiler_ происходит из-под контейнера. В таких ситуациях может помочь добавление `--tempdir .` к аргументам командной строки.

## Дорожная карта

* хранить кэш артефактов компиляции thrift файлов по их хэшам, чтобы не компилировать при каждом вызове;
* иметь возможность подсунуть директорию с уже готовыми артефактами компиляции;
* читать аргументы (service, function, args?) из stdin.
