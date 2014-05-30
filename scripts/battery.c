/*
 * battery.c
 *
 * Author: Chuck Valenza
 */

#include <stdio.h>
#include <stdlib.h>

//extern char **environ;

double batt_now, batt_full;
double pct;

int getval( const char* );

int main( void )
{
  batt_now = getval( "/sys/devices/LNXSYSTM:00/LNXSYBUS:00/PNP0A08:00/device:01/PNP0C09:00/PNP0C0A:00/power_supply/BAT0/energy_now" ) * 1.0;

  batt_full = getval( "/sys/devices/LNXSYSTM:00/LNXSYBUS:00/PNP0A08:00/device:01/PNP0C09:00/PNP0C0A:00/power_supply/BAT0/energy_full" ) * 1.0;

  pct = 100 * ( batt_now / batt_full );
  FILE* file;
  //GET_ENV("HOME");
  file = fopen( "charge_now", "w" );
  if ( !file ) {
    perror( "" );
    exit( EXIT_FAILURE );
  }
    
  fprintf( file, "| Batt: %0.1f%%", pct );
  fclose( file );

  return 0;
}

int getval( const char* file_name )
{
  FILE* file = fopen( file_name, "r" );
  int i = 0, value;

  fscanf( file, "%d", &i );
  while( !feof ( file ) )
  {
    value = i;
    fscanf( file, "%d", &i );
  }
  fclose( file );

return value;
}
