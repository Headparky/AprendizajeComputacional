# Guía para actualizar el repositorio.

## Introducción

Lo primero que hay que hacer al entrar al repo es ejecutar el siguiente comando:

```bash
git pull
```
Así conseguimos comprobar si ha habido algún cambio en el repositorio y descargarnos lo nuevo.

Una vez lo tenemos actualizado, cuando acabemos de trabajar en los ficheros que hayamos 
creado, modificado o eliminado, git tiene que saber que documentos quieres añadir al commit.

Un commit es como un checkpoint temporal, que después mediante la orden `git push` 
haremos que se reflejen los cambios en el repositorio.

A git tenemos que decirle que cambios queremos incluir en el commit.

```bash
git add <nombres-ficheros-cambios>
```
Lo más rápido es hacer `git add *`, aunque no es muy recomendable, ya que puede haber 
ficheros que no queramos incluir en el commit.

Para ver el estado actual de nuestro repositorio en local, podemos hacer `git status`, 
esto nos dará información acerca de a que ficheros estamos haciendo seguimiento y 
a cuales no.
