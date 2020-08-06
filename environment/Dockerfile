# hash:sha256:7952528813eeff2631d5d3a6addacf09f0336639adf16d4fe5318cfcca703020
FROM localhost:5000/codeocean/r-base:4.0.0-ubuntu18.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        pandoc=1.19.2.4~dfsg-1build4 \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'remotes::install_version("RSQLite", "2.2.0")' \
    && Rscript -e 'remotes::install_version("plotly", "4.9.2.1")'
