Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIARAWQ5EWIUYB2D6ML",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "eu-west-3"
)
library(aws.s3)
bucketlist()
put_object("data_paca.csv", bucket="s3buckext")
