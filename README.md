# Resaltador de sintaxis

1. Selecciona UN (1) lenguaje de programación que te resulte familiar (por ejemplo, C, C++, C#, Java, JavaScript, etc.), y determina las categorías léxicas  (por ejemplo, palabras reservadas, operadores, literales, comentarios, expresiones de asignación, funciones, etc.). Debes reunir, como mínimo 20 categorías léxicas, a parte, de la categoría léxica principal que reúne a las otras 20 dando la noción de un lenguaje de programación.
2. Define la BNF para representar todas las categorías léxicas del lenguaje seleccionado en el punto 1. Coloca tus representaciones en un archivo de texto con una estructura y organización que resulte conveniente para los siguientes pasos. Realiza la conversión a AFD usando JFlap.
3. Usando el lenguaje funcional Racket, implementa un motor de expresiones regulares que tome las expresiones definidas en el punto 2 y con ello esté en condiciones de escanear los elementos léxicos de cualquier archivo fuente provisto. Es decir, dada una entrada en un arcivo .txt conteniendo parte de código fuente escrito en el lenguaje seleccionado en el punto 1, tu programa será capaz de leer dicho archivo y analizarlo léxicamente.
4. Tu programa debe convertir su entrada en documentos de HTML+CSS que resalten su léxico. Cada léxico debe tener un color de letra diferente. En tu documento HTML+CSS, en la parte inferior, debe haber un bloque de fondo negro (simulando una consola), informando si el pedazo de código analizado es léxicamente correcto o no.
5. Utiliza las convenciones de codificación del lenguaje en el que está implementado tu programa.
6. Reflexiona sobre la solución planteada, los algoritmos implementados y sobre el tiempo de ejecución de estos.
7. Calcula la complejidad de tu algoritmo basada en el número de iteraciones y contrástala con el tiempo obtenido en el punto 6.
8. Plasma en un breve reporte de una página las conclusiones de tu reflexión en los puntos 6 y 7. Agrega además una breve reflexión sobre las implicaciones éticas que el tipo de tecnología que desarrollaste pudiera tener en la sociedad.

Esta actividad es realizada en grupos de hasta 4 miembros. Sin embargo, la carga de la entrega debe ser individual en eLumen.

Certifícate de que tu nombre esté en los comentarios del código fuente y en la lista de miembros de equipo en los otros material para ser considerado. Si no lo estuvieres, NO serás considerado y tu nota será cero (0).
