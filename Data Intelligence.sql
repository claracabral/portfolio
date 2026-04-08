DROP TABLE IF EXISTS Consultas;
DROP TABLE IF EXISTS Medicos;
DROP TABLE IF EXISTS Pacientes;

CREATE TABLE Medicos (id_medico INTEGER PRIMARY KEY, nome TEXT, especialidade TEXT);
CREATE TABLE Pacientes (id_paciente INTEGER PRIMARY KEY, nome TEXT, idade INTEGER, genero TEXT);
CREATE TABLE Consultas (
    id_consulta INTEGER PRIMARY KEY, 
    id_paciente INTEGER, 
    id_medico INTEGER, 
    data_consulta DATE, 
    valor_consulta REAL
);

INSERT INTO Medicos VALUES 
(1, 'Dr. Arnaldo', 'Cardiologia'), 
(2, 'Dra. Julia', 'Clínica Geral'), 
(3, 'Dr. Paulo', 'Ortopedia'),
(4, 'Dra. Beatriz', 'Pediatria'),
(5, 'Dr. Marcos', 'Neurologia');

-- Inserindo uma amostra de 20 para testar agora, você pode repetir mudando o ID
INSERT INTO Pacientes (id_paciente, nome, idade, genero) VALUES 
(101, 'Ana Silva', 28, 'F'), (102, 'Bruno Costa', 52, 'M'), (103, 'Carla Dias', 33, 'F'),
(104, 'Daniel Rebouças', 61, 'M'), (105, 'Elena Ramos', 22, 'F'), (106, 'Fabio Jr', 45, 'M'),
(107, 'Gisele B', 39, 'F'), (108, 'Hugo Souza', 19, 'M'), (109, 'Iara Lima', 70, 'F'),
(110, 'João Pedro', 12, 'M'), (111, 'Karen F', 31, 'F'), (112, 'Luis Melo', 55, 'M'),
(113, 'Mara Luz', 42, 'F'), (114, 'Nair Silva', 81, 'F'), (115, 'Otavio G', 37, 'M'),
(116, 'Paula A', 26, 'F'), (117, 'Queiroz M', 48, 'M'), (118, 'Rosa X', 63, 'F'),
(119, 'Sonia V', 51, 'F'), (120, 'Tiago L', 29, 'M');

-- Simulando 30 consultas (alguns pacientes repetidos para termos "fidelidade")
INSERT INTO Consultas (id_consulta, id_paciente, id_medico, data_consulta, valor_consulta) VALUES
(1, 101, 1, '2026-01-10', 300), (2, 101, 1, '2026-02-15', 250), (3, 102, 2, '2026-01-20', 200),
(4, 103, 2, '2026-02-05', 200), (5, 103, 2, '2026-03-05', 200), (6, 104, 3, '2026-01-12', 350),
(7, 105, 1, '2026-03-20', 300), (8, 106, 4, '2026-01-15', 150), (9, 106, 4, '2026-02-20', 150),
(10, 107, 5, '2026-03-01', 400), (11, 108, 2, '2026-01-25', 200), (12, 109, 1, '2026-02-10', 300),
(13, 110, 4, '2026-01-30', 150), (14, 111, 3, '2026-03-15', 350), (15, 112, 2, '2026-02-12', 200),
(16, 113, 5, '2026-01-18', 400), (17, 114, 1, '2026-03-22', 300), (18, 115, 3, '2026-02-25', 350),
(19, 116, 2, '2026-01-05', 200), (20, 117, 5, '2026-03-10', 400), (21, 118, 1, '2026-02-28', 300),
(22, 119, 2, '2026-01-14', 200), (23, 120, 3, '2026-03-08', 350), (24, 101, 1, '2026-03-15', 250),
(25, 103, 2, '2026-04-05', 200), (26, 106, 4, '2026-03-25', 150), (27, 112, 2, '2026-03-12', 200),
(28, 115, 3, '2026-04-01', 350), (29, 110, 4, '2026-02-28', 150), (30, 108, 2, '2026-03-02', 200);


-- Inserindo dados estratégicos para análise
INSERT INTO Medicos VALUES (1, 'Dr. Arnaldo', 'Cardiologia'), (2, 'Dra. Julia', 'Clínica Geral'), (3, 'Dr. Paulo', 'Ortopedia');

INSERT INTO Pacientes VALUES 
(101, 'Ana Souza', 28, 'F'), (102, 'Bruno Lima', 52, 'M'), 
(103, 'Carla Dias', 33, 'F'), (104, 'Daniel Silva', 61, 'M'),
(105, 'Elena Ramos', 22, 'F');

INSERT INTO Consultas VALUES 
(1, 101, 1, '2026-01-10', 300.00), (2, 101, 1, '2026-02-15', 250.00), -- Ana voltou (Fiel)
(3, 102, 2, '2026-01-20', 200.00), -- Bruno só foi 1 vez (Churn)
(4, 103, 2, '2026-02-05', 200.00), (5, 103, 2, '2026-03-05', 200.00), -- Carla voltou (Fiel)
(6, 104, 3, '2026-01-12', 350.00), -- Daniel só foi 1 vez (Churn)
(7, 105, 1, '2026-03-20', 300.00); -- Elena só foi 1 vez (Churn)


-- IDENTIFICAR PACIENTES QUE NÃO VOLTARAM
SELECT 
    p.nome AS paciente,
    p.idade,
    m.especialidade AS area_atendida,
    c.data_consulta AS data_da_visita
FROM Pacientes p
JOIN Consultas c ON p.id_paciente = c.id_paciente
JOIN Medicos m ON c.id_medico = m.id_medico
GROUP BY p.id_paciente
HAVING COUNT(c.id_consulta) = 1;


-- TAXA DE RECORRÊNCIA POR ESPECIALIDADE
SELECT 
    m.especialidade,
    COUNT(DISTINCT c.id_paciente) AS pacientes_unicos,
    COUNT(c.id_consulta) AS total_consultas,
    ROUND(CAST(COUNT(c.id_consulta) AS FLOAT) / COUNT(DISTINCT c.id_paciente), 2) AS indice_recorrencia
FROM Medicos m
LEFT JOIN Consultas c ON m.id_medico = c.id_medico
GROUP BY m.especialidade
ORDER BY indice_recorrencia DESC;


-- FATURAMENTO TOTAL POR MÉDICO
SELECT 
    m.nome AS medico,
    m.especialidade,
    SUM(c.valor_consulta) AS faturamento_total,
    COUNT(c.id_consulta) AS total_atendimentos
FROM Medicos m
JOIN Consultas c ON m.id_medico = c.id_medico
GROUP BY m.id_medico
ORDER BY faturamento_total DESC;


-- PERFIL DEMOGRÁFICO
SELECT 
    m.especialidade,
    AVG(p.idade) AS media_idade_pacientes,
    MIN(p.idade) AS idade_minima,
    MAX(p.idade) AS idade_maxima
FROM Medicos m
JOIN Consultas c ON m.id_medico = c.id_medico
JOIN Pacientes p ON c.id_paciente = p.id_paciente
GROUP BY m.especialidade;
