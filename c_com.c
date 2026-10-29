#include <stdio.h>
#include <windows.h>

static HANDLE hSerial;

int com_open( const char * name )
{
    hSerial = CreateFile(
        name,
        GENERIC_READ | GENERIC_WRITE,
        0,             // Must be opened with exclusive-access
        NULL,          // No security attributes
        OPEN_EXISTING, // Must use OPEN_EXISTING
        0,             // 0 for synchronous/blocking I/O
        NULL
    ); // hTemplateMust be NULL for comm devices

    if ( hSerial == INVALID_HANDLE_VALUE ) {
        DWORD err = GetLastError();
        if ( err == ERROR_FILE_NOT_FOUND ) {
            printf( "Error: Port %s does not exist.\n", name );
        } else {
            printf( "Error opening port: %lu\n", err );
        }
        return 0;
    }

    // 2. Configure State Settings (Baud rate, byte size, stop bits, parity)
    DCB dcbSerialParams = { 0 };
    dcbSerialParams.DCBlength = sizeof( dcbSerialParams );

    if ( !GetCommState( hSerial, &dcbSerialParams ) ) {
        printf( "Error getting device state\n" );
        CloseHandle( hSerial );
        return 0;
    }

    dcbSerialParams.BaudRate = CBR_9600;
    dcbSerialParams.ByteSize = 7;
    dcbSerialParams.StopBits = ONESTOPBIT;
    dcbSerialParams.Parity = EVENPARITY;

    if ( !SetCommState( hSerial, &dcbSerialParams ) ) {
        printf( "Error setting device parameters\n" );
        CloseHandle( hSerial );
        return 0;
    }

    // 3. Configure Timeouts (Crucial so ReadFile doesn't block forever)
    COMMTIMEOUTS timeouts = { 0 };
    timeouts.ReadIntervalTimeout = MAXDWORD; // Max time between incoming bytes (ms)
    timeouts.ReadTotalTimeoutConstant = 0;   // Total timeout constant for read (ms)
    timeouts.ReadTotalTimeoutMultiplier = 0; // Multiplier per byte requested

    timeouts.WriteTotalTimeoutConstant = 50;   // Total timeout constant for write (ms)
    timeouts.WriteTotalTimeoutMultiplier = 10; // Multiplier per byte requested

    if ( !SetCommTimeouts( hSerial, &timeouts ) ) {
        printf( "Error setting timeouts\n" );
        CloseHandle( hSerial );
        return 0;
    }

    return 1;
}

int com_write( const unsigned char * data, int len )
{
    DWORD bytesWritten = 0;
    if ( !WriteFile( hSerial, data, len, &bytesWritten, NULL ) ) {
        printf( "Error writing to port\n" );
    }
    return bytesWritten;
}

int com_read( unsigned char * data, int len )
{
    DWORD bytesRead = 0;

    if ( !ReadFile( hSerial, data, len, &bytesRead, NULL ) ) {
        printf( "Error reading from port\n" );
    }
    return bytesRead;
}

int com_read_byte()
{
    unsigned char b;
    if ( com_read( &b, 1 ) ) {
        return b;
    } else
        return -1;
}

void com_close()
{
    CloseHandle( hSerial );
}

// int main()
// {
//     com_open( "COM3" );
// 
//     const char * cmd = "\x02"
//                        "00E0202"
//                        "\x03"
//                        "6C";
//     // const char * cmd = "\x05";
//     printf( "writing\n" );
//     com_write( cmd, strlen( cmd ) );
// 
//     Sleep( 1000 );
// 
//     static char buffer[ 1024 ];
//     printf( "reading\n" );
//     int len = com_read( buffer, 1024 );
// 
//     printf( "plc: \"%.*s\" hex: ", len, buffer );
// 
//     for ( int i = 0; i < len; i++ ) {
//         printf( "%02x ", (unsigned char) buffer[ i ] );
//     }
//     printf( "\n" );
// 
//     com_close();
//     return 0;
// }
