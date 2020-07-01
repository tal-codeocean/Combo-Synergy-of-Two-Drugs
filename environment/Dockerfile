# hash:sha256:2b1c5b424e029d2c3aa03a965464b2da415e184c6f062e9d3b5e777b65f05cf9
FROM localhost:5000/codeocean/r-base:4.0.0-ubuntu18.04

ARG DEBIAN_FRONTEND=noninteractive

RUN Rscript -e 'remotes::install_version("RSQLite", "2.2.0")' \
    && Rscript -e 'remotes::install_version("plotly", "4.9.2.1")'
