## mime-mail-ses

Send mime-mail messages via Amazon SES

### send-aws

The build target `mime-mail-ses:exe:send-aws` is a command line executable that
allows you to send an e-mail via Amazon SES. Most parameters are supplied
through command line options; the AWS secret key and the body of the message are
read from standard input.

Example:

    % cabal run :send-aws -- \
        --subject 'Checking if AWS works.' \
        --from '<i>your e-mail address</i>' \
        --to '<i>your e-mail address</i>' \
        --key '<i>your key ID</i>' \
        --region 'us-east-2'
    Up to date
    Enter AWS secret: <i>your secret key</i>
    Enter message below.
    This is a test letter sent from command line.
