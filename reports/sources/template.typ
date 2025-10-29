#import "@preview/zebraw:0.5.5": *

#let font_size = 14pt;
#let code_font_size = 10pt;

#let conf(
  title: none,
  num: none,
  author: (
    group: none,
    name: none
  ),
  lecturer: (
    name: none
  ),
  doc
) = {
  set page(
    margin: (
      right: 1.5cm,
      rest: 2.0cm,
    )
  )

  set text(
    font: "Times New Roman",
    size: font_size
  )
  // Титульный лист
  align(center)[
    *Московский государственный технический* \
    *университет им. Н. Э. Баумана* \

    #v(font_size * 3)

    Факультет «Информатика и системы управления» \
    Кафедра ИУ5 «Системы обработки информации и управления» \

    #v(font_size * 3)

    Курс «Парадигмы и конструкции языков программирования» \
    Отчет по лабораторной работе №#num \
    «#title»
  ]

  v(font_size * 11)

  align(right)[
    Выполнил: \
    Студент группы #author.group \
    #author.name
  ]

  v(font_size)

  align(right)[
    Проверил: \
    #lecturer.name
  ]

  v(font_size * 14)

  align(center)[
    #datetime.today().year() г.
  ]

  // Основной контент
  doc
}

// Секции отчета
#let task(content) = {
  heading[Задание]
  content
}

#let code_heading = heading[Листинг программы]

#let code(title: none, content) = {
  zebraw(
    header: title,
    text(size: code_font_size)[#content]
  )
}

#let examples_heading = heading[Примеры выполнения]

#let terminal_example(title: none, content) = {
  zebraw(
    header: title,
    numbering: false,
    highlight-lines: 1,
    text(size: code_font_size)[#content]
  )
}