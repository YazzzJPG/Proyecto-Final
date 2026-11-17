# Proyecto Final de Estadística

**Análisis estadístico inferencial de la disponibilidad tecnológica y la aceptación de plataformas virtuales entre estudiantes**

Universidad Autónoma de Ciudad Juárez · Instituto de Ingeniería y Tecnología
Profesor: Liliana Orizel Martínez Martínez

## Equipo

| Integrante | Matrícula |
|---|---|
| Frida Yazmín López Nava | 259790 |
| Alexis Abdias Posadas González | 257537 |
| Luis Raúl Rivera Avalos | 257743 |

## ¿Qué contiene?

| Archivo | Descripción |
|---|---|
| `analisis_inferencial.R` | Script con todo el análisis (estimación, intervalos de confianza, pruebas de hipótesis y bondad de ajuste). |
| `datos_limpios.csv` | Base de datos usada en el análisis (frecuencias por pregunta y categoría). |
| `datos_originales.csv` | Base de datos original, antes de la limpieza. |
| `resultados/` | Tablas (`.csv`) y figuras (`.png`) que genera el script. |

El reporte técnico se entrega aparte (`Reporte_Final_Estadistica_Inferencial.pdf` y `.docx`).

## ¿Cómo se ejecuta?

1. Abrir R o RStudio (versión 4.6.1; no requiere instalar paquetes).
2. Poner la carpeta `analisis` como directorio de trabajo
   (en RStudio: *Session > Set Working Directory > To Source File Location*).
3. Abrir `analisis_inferencial.R` y presionar **Source** (o `Ctrl + Shift + S`).
4. Al terminar, la consola muestra "Listo" y se crea la carpeta `resultados/`.

## Resultados que genera

- `estimacion_ic.csv`: proporciones estimadas e intervalos de confianza (Wald y Wilson).
- `pruebas_hipotesis.csv`: cuatro pruebas z para una proporción (H1 a H4).
- `gof_complejidad.csv` y `gof_resumen.csv`: bondad de ajuste chi-cuadrada de la complejidad percibida.
- `simulacion_supuestos.csv`: verificación de supuestos por simulación.
- `fig06_pruebas_hipotesis.png` y `fig07_bondad_ajuste.png`: figuras del reporte.

## Datos del estudio

Población N = 17,000 estudiantes; muestra n = 1,231. Nivel de significancia α = 0.05.
