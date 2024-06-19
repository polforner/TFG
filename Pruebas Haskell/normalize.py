import string
import unidecode
import re


def texto_a_ascii(texto):
    texto_ascii = unidecode.unidecode(texto)
    texto_sin_puntuacion = texto_ascii.translate(
        str.maketrans(string.punctuation, '.' * len(string.punctuation)))
    texto_sin_puntuacion_espacios = re.sub(
        r'\.(\s|$)', ' . ', texto_sin_puntuacion)
    texto_sin_saltos_de_linea = texto_sin_puntuacion_espacios.replace(
        '\n', ' ')
    texto_sin_dobles_espacios = re.sub(' +', ' ', texto_sin_saltos_de_linea)
    texto_sin_puntuacion_minusculas = texto_sin_dobles_espacios.lower()
    return texto_sin_puntuacion_minusculas


# Abre el archivo de entrada y lee el texto
with open(r'C:\Users\polfo\Documents\UNI\TFG\Pruebas Haskell\quijote.txt', 'r', encoding='utf-8') as f:
    texto_entrada = f.read()

# Procesa el texto
resultado = texto_a_ascii(texto_entrada)

n = 10000
# Abre el archivo de salida y escribe el resultado
with open(r'C:\Users\polfo\Documents\UNI\TFG\Pruebas Haskell\salida.txt', 'w', encoding='utf-8') as f:
    f.write(resultado[:n])
