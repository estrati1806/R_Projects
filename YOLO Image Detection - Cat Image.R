# image.darknet is an Image classification and Object Detection package for R users
# It provides image classification with deep learning models like AlexNet, Darknet, VGG-16, Extraction (GoogleNet) and Darknet19
# and object detection using the state-of-the art YOLO detection system

# The YOLO detection system proposes using an end-to-end neural network that makes predictions of bounding boxes and class probabilities all at once

# install.packages("image.darknet", repos = "https://bnosac.github.io/drat")
library(image.darknet)

# Cat Image
darknet_model_c <- image_darknet_model(type = "classify",
                                     model = "tiny.cfg",
                                     weights = system.file(package = "image.darknet", "models", "tiny.weights"),
                                     labels = system.file(package = "image.darknet", "include", "darknet", "data", "imagenet.shortnames.list"))
image_darknet_classify(file = "C:/Users/User/Desktop/github/Neural Networks/images/cat.jpg",
                       object = darknet_model_c)

# Detect
darknet_model_d <- image_darknet_model(type = "detect",
                                     model = "tiny-yolo-voc.cfg",
                                     weights = system.file(package = "image.darknet", "models", "tiny-yolo-voc.weights"),
                                     labels = system.file(package = "image.darknet", "include", "darknet", "data", "voc.names"))
image_darknet_detect(file = "C:/Users/User/Desktop/github/Neural Networks/images/cat.jpg",
                     object = darknet_model_d)

# Console:
# C:/Users/User/Desktop/github/Neural Networks/images/cat.jpg: Predicted in 3.302000 seconds.
# Boxes: 845 of which 1 above the threshold.
# cat: 91%
