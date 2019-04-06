## 0.5

* Add support for inline images and multipart/related
* Add `filePart` and `filePartBS` for attachments
* Change `Part` data type (`Disposition` and `PartContent`)
* Remove `addAttachmentCid` and `addAttachmentBSCid` (replaced by `addImage`)
* Remove `getAttachmentPartBS` and `getAttachmentPart`

## 0.4.14

* Add `IsString` instance for `Address`

## 0.4.13.1

* Fix [#50](https://github.com/snoyberg/mime-mail/issues/50): space between name and address [#51](https://github.com/snoyberg/mime-mail/pull/51)

## 0.4.13

* Sanitize headers to protect against email injection.

## 0.4.12

* Add function to add attachments with content id [#48](https://github.com/snoyberg/mime-mail/pull/48)

## 0.4.11

* Export renderAddress as a utility (e.g. Reply-to) [#44](https://github.com/snoyberg/mime-mail/pull/44)

## 0.4.10

* addParts: append mail parts to a Mail [#43](https://github.com/snoyberg/mime-mail/pull/43)

## 0.4.9

* Add `sendmailCustomCaptureOutput` [#42](https://github.com/snoyberg/mime-mail/pull/42)

## 0.4.8.1

* Bump blaze-builder upper bound [#39](https://github.com/snoyberg/mime-mail/pull/39)

## 0.4.8

* Add some `Eq` instances [#38](https://github.com/snoyberg/mime-mail/pull/38)

## 0.4.7

* `simpleMailInMemory`

## 0.4.6.1

Add a soft line break when hitting a QPEscape at the end of an encoded line. [#34](https://github.com/snoyberg/mime-mail/pull/34)
