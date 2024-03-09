# VM et Compilateur en LISP

Ce projet consiste en une machine virtuelle (VM) à registres ainsi qu'un compilateur, tous deux implémentés en Common LISP.  
La VM exécute un langage assembleur tandis que le compilateur traduit du LISP vers ce langage assembleur.

## Structure du Projet

- **docs/** : Contient la documentation utile à la réalisation du projet.
- **src/** : Contient le code source.
    - **compilation/** : Contient le code source du compilateur.
    - **VM/** : Contient le code source de la machine virtuelle.
    - **test/** : Contient les fichiers de tests.
- **main.lisp** : Point d'entrée du programme.
- **README.md** : Ce fichier.


## Instructions
1. Installer Common LISP sur le site https://clisp.sourceforge.io/  
    Disponible pour Windows, Mac et Linux
   
3. Clonez ce projet :  
```shell
git clone git@github.com:eric-gilles/VM_Compiler_LISP.git
```
4. Lancer CLISP :
```shell
clisp
```
5. Charger le fichier main.lisp pour avoir un exemple d'utilisation
```lisp
[1]> (load "main.lisp")
```
### Alternative Replit :
1. Se connecter à Replit : https://replit.com/login
2. Fork le projet via ce <a href="https://replit.com/@eric-gilles/ProjetCompil?v=1">lien</a>. Cliquer sur **Fork & Run**.
3. Lancer en appuyant sur **RUN** en haut de votre projet.


## Auteurs

Ce projet a été réalisé par :
- [Eric GILLES](https://github.com/eric-gilles)
- [Morgan Navel](https://github.com/MorganNavel)
