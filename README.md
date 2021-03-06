# Datamining  - TP 2 - Reglas de asociación

## Enlaces

* [Enunciado](https://github.com/magistery-tps/dm-tp2/blob/master/docs/enunciado.pdf)
* [Informe](https://github.com/magistery-tps/dm-tp2/blob/master/docs/Data%20Mining%20-%20TP%20N%C2%B02%20-%20Grupo%205.pdf)
* [Guía: Reglas de asociación con arules (R)](https://github.com/dm-uba/dm-uba.github.io/blob/master/2021/laboratorios/LAB09/guias/guia-reglas-asociacion-arules.md)

## Descargar repositorio

**Paso 1**: Instalar [git](https://git-scm.com/downloads).

**Paso 2**:  Ahora si  clonamos el repositorio.

```bash
$ git clone https://github.com/mastery-tps/dm-tp2.git
$ cd dm-tp2
```

## Importar dataset en mongodb

**Paso 1**: Desde la consola y parados en el directorio del proyecto, descomprimimos `data.rar`:

```bash
$ rar x data.rar
```

**Paso 2**: Antes de seguir es necesario tener instalado [mongodb](https://www.mongodb.com/try/download/community) y [mongodb-tools](https://www.mongodb.com/try/download/database-tools).

**Paso 3**: Importamos todas las colecciones en la base de datos:

```bash
$ mongoimport -d spotify -c lyrics --file lyrics.json --jsonArray
$ mongoimport -d spotify -c track_features_top_200       --file track_features_top_200.json       --jsonArray
$ mongoimport -d spotify -c track_features_top_10_lyric  --file track_features_top_10_lyric.json  --jsonArray
$ mongoimport -d spotify -c track_features_top_50_lyric  --file track_features_top_50_lyric.json  --jsonArray
$ mongoimport -d spotify -c track_features_top_200_lyric --file track_features_top_200_lyric.json --jsonArray
```

## Vistas y nuevas colecciones

[Ver views.js](https://github.com/magistery-tps/dm-tp2/blob/master/views.js)

## Start/Stop MongoDB

### Linux:

```bash
$ sudo systemctl restart mongod
```

### MacOS

```bash
$ brew services start mongodb-community
$ brew services stop  mongodb-community
```
