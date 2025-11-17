# Dev container for Rascal + TypePal development in GitHub Codespaces
# Ahora usando Java 21 para que la extensión de Java no se queje.

FROM mcr.microsoft.com/devcontainers/java:1-21-bullseye

# Instalamos Maven explícitamente (la imagen suele traerlo, pero así queda claro)
RUN apt-get update \
    && apt-get install -y --no-install-recommends maven \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Directorio de trabajo por defecto dentro del contenedor
WORKDIR /workspaces
