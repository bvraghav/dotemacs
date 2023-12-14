#! /bin/zsh

# conda install --yes   \

conda create -n emacs   \
beautifulsoup4 \
click		\
curl		\
ffmpeg		\
flask		\
gflags		\
glog		\
gstreamer	\
h5py		\
imageio		\
jedi		\
jsonschema	\
jupyter		\
markdown	\
matplotlib	\
mlflow		\
ncurses		\
nettle		\
ninja		\
numpy		\
pandas		\
pcre		\
pillow		\
pip		\
prompt-toolkit	\
protobuf	\
python \
querystring_parser \
re2 \
readline \
regex \
requests \
scikit-image \
scikit-learn \
scipy \
soupsieve \
sqlite=3 \
tk \
tqdm \
xz \
yaml \



conda activate emacs

conda install --yes \
    -c conda-forge \
    dominate	\
    kornia		\


conda install --yes pytorch torchvision torchaudio cpuonly -c pytorch


conda install --yes lightning -c conda-forge
