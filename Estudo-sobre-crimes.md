# Instalar pacotes

library(tidyverse) library(dplyr) library(ggplot2)

# Carregando os dados

dados\_seguranca &lt;- read\_csv(‘/Users/felipesilveira/RStudio/Estudo
sobre crimes/br\_fbsp\_absp\_municipio.csv’)

# Verificando estrutura

glimpse(dados\_seguranca)

# Primeiras linhas

head(dados\_seguranca)

# Resumo estatístico

summary(dados\_seguranca)

# Transformando algumas colunas em tipo factor

dados\_limpos &lt;- dados\_seguranca |&gt; mutate( ano = as.factor(ano),
sigla\_uf = as.factor(sigla\_uf), grupo = as.factor(grupo) )

## EDA

# 1. Como as mortes violentas evoluíram ao longo dos anos no Brasil?

# Agrupar por ano e somar mortes violentas

evolucao\_brasil &lt;- dados\_limpos |&gt; group\_by(ano) |&gt;
summarise( total\_mortes\_violentas =
sum(quantidade\_mortes\_violentas\_intencionais, na.rm = TRUE),
total\_homicidios = sum(quantidade\_homicidio\_doloso, na.rm = TRUE) )

print(evolucao\_brasil)

# Visualização de mortes violentas

ggplot(evolucao\_brasil, aes(x = ano, y = total\_mortes\_violentas,
group = 1)) + geom\_line(color = “red”, size = 1.5) + geom\_point(color
= “red”, size = 3) + labs( title = “Evolução das mortes violentas
intencionais no Brasil(Municípios da amostra)”, x = “Ano”, y =
“Quantidade total” ) + theme\_minimal()

# 2. Quais estados tiveram mais homicídios dolosos no último ano registrado?

# Encontrar último ano

ultimo\_ano &lt;- max(as.numeric(as.character(dados\_limpos$ano)))

# Filtrar pelo ano, agrupar por UF e ordenar

ranking\_uf &lt;- dados\_limpos |&gt; filter(ano == ultimo\_ano) |&gt;
group\_by(sigla\_uf) |&gt; summarise( total\_homicidios =
sum(quantidade\_homicidio\_doloso, na.rm = TRUE) ) |&gt;
arrange(desc(total\_homicidios))

print(ranking\_uf)

# Gráfico de barras com os 10 primeiros

ggplot(head(ranking\_uf, 10), aes(x = reorder(sigla\_uf,
total\_homicidios), y = total\_homicidios)) + geom\_bar(stat =
“identity”, fill = “steelblue”) + coord\_flip() + labs( title =
paste(“Top 10 UFs por homicídio doloso em”, ultimo\_ano), x = “UF”, y =
“Quantidade total de homicídios” ) + theme\_bw()

\#Existe relação entr roubo de veículos e tráfico de entorpecentes? \#
Selecionar colunas e remover NAs dados\_correlacao &lt;- dados\_limpos
|&gt; select( quantidade\_roubo\_furto\_veiculos,
quantidade\_trafico\_entorpecente ) |&gt; na.omit()

# Calcular a correlação

cor(dados\_correlacao*q**u**a**n**t**i**d**a**d**e*<sub>*r*</sub>*o**u**b**o*<sub>*f*</sub>*u**r**t**o*<sub>*v*</sub>*e**i**c**u**l**o**s*, *d**a**d**o**s*<sub>*c*</sub>*o**r**r**e**l**a**c**a**o*quantidade\_trafico\_entorpecente)

# Visualizar a correlação com um gráfico de dispersão (scatterplot)

ggplot(dados\_correlacao, aes(x = quantidade\_roubo\_furto\_veiculos, y
= quantidade\_trafico\_entorpecente)) + geom\_point(alpha = 0.5) + \#
alpha = transparência geom\_smooth(method = “lm”, color = “blue”) + \#
Adiciona uma linha de tendência labs( title = “Correlação entre
Roubo/Furto de Veículos e Tráfico”, x = “Roubo e Furto de Veículos”, y =
“Tráfico de Entorpecentes” ) + theme\_light()

# Total de feminicidios

total\_feminicidios &lt;- dados\_limpos |&gt; summarise( total\_geral =
sum(quantidade\_feminicidio, na.rm = TRUE) )

print(total\_feminicidios)

# Total por estado

# Quantidade de feminicidios por ano

feminicidios\_por\_ano &lt;- dados\_limpos |&gt; group\_by(ano) |&gt;
summarise( total\_anual = sum(quantidade\_feminicidio, na.rm = TRUE) )
|&gt; arrange(ano)

# Visualizando em gráficos

ggplot(feminicidios\_por\_ano, aes(x = as.factor(ano), y = total\_anual,
group = 1)) + geom\_line(color = “purple”, size = 1.2) +
geom\_point(color = “purple”, size = 2.5) + labs( title = “Evolução dos
Feminicídios por Ano (na amostra)”, x = “Ano”, y = “Quantidade Total de
Feminicídios” ) + theme\_minimal()
