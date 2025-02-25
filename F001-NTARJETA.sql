CREATE DEFINER=`root`@`localhost` FUNCTION `GenerarNumeroTarjeta`() RETURNS char(16) CHARSET utf8mb4
    DETERMINISTIC
BEGIN
    DECLARE numero_base CHAR(15);
    DECLARE digito_verificacion INT;
    DECLARE suma_total INT DEFAULT 0;
    DECLARE i INT;
    DECLARE digito INT;
    DECLARE temp INT;
    
    -- Generar un número aleatorio de 11 dígitos + prefijo "4539" (Visa)
    SET numero_base = CONCAT('4539', LPAD(FLOOR(RAND() * 1000000000000), 11, '0'));

    -- Algoritmo de Luhn para calcular el dígito de verificación
    SET suma_total = 0;
    
    -- Recorrer los 15 dígitos generados
    SET i = 15;
    WHILE i > 0 DO
        SET digito = SUBSTRING(numero_base, i, 1);
        
        -- Multiplicar por 2 los dígitos en posiciones impares desde la derecha
        IF (15 - i) % 2 = 0 THEN
            SET temp = digito * 2;
            IF temp > 9 THEN
                SET temp = temp - 9;
            END IF;
        ELSE
            SET temp = digito;
        END IF;

        -- Sumar al total
        SET suma_total = suma_total + temp;
        
        SET i = i - 1;
    END WHILE;
    
    -- Calcular el dígito de verificación
    SET digito_verificacion = (10 - (suma_total % 10)) % 10;

    -- Devolver el número completo de 16 dígitos
    RETURN CONCAT(numero_base, digito_verificacion);
END