## 0.4.2

* Add executable `send-aws` for sending e-mails via Amazon SES from command line.
* Update Signature Version for Amazon SES from 3 to 4.

## 0.4.1

* Add 'renderSendMailSESGlobal' and 'sendMailSESGlobal' function which uses the global manager.

## 0.4.0.0

* Support IAM temp credentials. This is a backwards-incompatible change that adds
  a field to the SES type to represent (optional) session tokens when using roles
  attached to EC2 instances.
* Make fields in the `SES` data type strict to promote warnings to errors.

## 0.3.2.3

http-client 0.5 support

## 0.3.2.2

`time` 1.5 support

## 0.3.2

Expose `SESException` datatype and constructor.
