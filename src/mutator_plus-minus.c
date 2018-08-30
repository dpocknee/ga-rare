/* mutator_plus-minus.exe input_file_name output_file_name random_seed percentage_chance_of_value_changing maximum_change_of_value

Takes in an input .wav file (input_file_name) and adjusts values in the file be a random amount up or down.  Then outputs this adjusted file (output_file_name) 
Changes a set percentage of the values in a given file up or down by a specified amount.
This main function takes four arguments when called via the command line:
- [1] input file name
- [2] output file name
- [3] random seed
- [4] Percentage chance of value changing (out of 100) (int)
- [5] minimum change in integer values.  (minimum 0)
- [6] maximum change in integer values.  (maximum 32652)
        These are the minimum and  maximum amount a integer can be moved up or down.  
	e.g. minimum 0 and maximum 8 could adjust values anywhere between +8 and -8
	but minimum 3 and maximum 7 would only select numbers adjust values by  between +3 to +7 and -3 to -7.
	This makes it easier to push the program to make more extreme changes.
*/


#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include "make_wav.h"
#include <time.h>

/* The write_little_endian and write_wav functions as well as make_wav.h are taken from 
 * Kevin Karplus's make_wav.c code
 * Fri Jun 18 16:36:23 PDT 2010 
 * Creative Commons license Attribution-NonCommercial
 *  http://creativecommons.org/licenses/by-nc/3.0/
  */


void write_little_endian(unsigned int word, int num_bytes, FILE *wav_file)
{
    unsigned buf;
    while(num_bytes>0)
    {   buf = word & 0xff;
        fwrite(&buf, 1,1, wav_file);
        num_bytes--;
    word >>= 8;
    }
}


void write_wav(char * filename, unsigned long num_samples, short int * data, int s_rate)
{
    FILE* wav_file;
    unsigned int sample_rate;
    unsigned int num_channels;
    unsigned int bytes_per_sample;
    unsigned int byte_rate;
    unsigned long i;    /* counter for samples */

    num_channels = 1;   /* monoaural */
    bytes_per_sample = 2;

    if (s_rate<=0) sample_rate = 44100;
    else sample_rate = (unsigned int) s_rate;

    byte_rate = sample_rate*num_channels*bytes_per_sample;

    wav_file = fopen(filename, "wb");
    assert(wav_file);   /* make sure it opened */

    /* write RIFF header */
    fwrite("RIFF", 1, 4, wav_file);
    write_little_endian(36 + bytes_per_sample* num_samples*num_channels, 4, wav_file);
    fwrite("WAVE", 1, 4, wav_file);

    /* write fmt  subchunk */
    fwrite("fmt ", 1, 4, wav_file);
    write_little_endian(16, 4, wav_file);   /* SubChunk1Size is 16 */
    write_little_endian(1, 2, wav_file);    /* PCM is format 1 */
    write_little_endian(num_channels, 2, wav_file);
    write_little_endian(sample_rate, 4, wav_file);
    write_little_endian(byte_rate, 4, wav_file);
    write_little_endian(num_channels*bytes_per_sample, 2, wav_file);  /* block align */
    write_little_endian(8*bytes_per_sample, 2, wav_file);  /* bits/sample */

    /* write data subchunk */
    fwrite("data", 1, 4, wav_file);
    write_little_endian(bytes_per_sample* num_samples*num_channels, 4, wav_file);
    for (i=0; i< num_samples; i++)
    {   write_little_endian((unsigned int)(data[i]),bytes_per_sample, wav_file);
    }

    fclose(wav_file);
}


/* My code*/

#define S_RATE  (44100)
#define BUF_SIZE (S_RATE*1) /* x second buffer */
#define BitRate (65536)
#define HEADER_SIZE (22) // this is the header size (actually 44 bytes, but in 22 8 byte blocks)
#define FILE_SIZE (BUF_SIZE+HEADER_SIZE)


int rnd(int range);
void seedrnd(void);
int randshuffler(int pool, int total);

int main(int argc, char* argv[])
{
  int minimum_change_value = atoi(argv[5]);
  int maximum_change_value = atoi(argv[6]);
  if(argc<7) { printf("Error: Not enough variables! (%i)", argc);}
  else if (argc>7) {printf("Error: Too many variables (%i)", argc);}
  else if (maximum_change_value < minimum_change_value) {printf("Error: your maximum is below your minimum");}
  else {
    // This has been optimized for speed.
    char input_file_name[40], output_file_name[40];
    short int buffer_file_contents[FILE_SIZE],new_buffer[BUF_SIZE];

    int input_file_position,output_buffer_position,randseed;
    int chance_of_change = atoi(argv[4]);
    int difference_change_value, change_polarity;
    int random_sample, top_distance, bottom_distance, lowest_random, new_value;
    short int current_value;
    int top_value = 32767;
    int bottom_value = -32767;
	
    sprintf(input_file_name, "%s",argv[1]);
    sprintf(output_file_name, "%s",argv[2]);

    FILE * input_file = fopen(input_file_name, "rb");
    FILE * output_file = fopen(output_file_name, "wb");

    // Seed random number generator
    srand(atoi(argv[3]));

    // Copy all of file to buffer
    fread(buffer_file_contents, sizeof(short int), FILE_SIZE,input_file);

    for(input_file_position=HEADER_SIZE;input_file_position<FILE_SIZE;input_file_position++)
      {
	output_buffer_position = input_file_position-HEADER_SIZE;
	//random change variable:
	random_sample = randshuffler(5,100);
	current_value = buffer_file_contents[input_file_position];
	if (random_sample <= chance_of_change)
	  {
	    //The new value moved up or down a random amount:
	    top_distance = top_value - current_value;
	    bottom_distance = current_value - bottom_value;

	    if(top_distance < maximum_change_value) {lowest_random = current_value - ((maximum_change_value*2)-top_distance);}
	    else if (bottom_distance < maximum_change_value) { lowest_random = bottom_value;}
	    else { lowest_random = current_value - maximum_change_value;}
		
	    difference_change_value = maximum_change_value - minimum_change_value;
	    change_polarity = randshuffler(5,2);
	    if(change_polarity == 0) {
	      new_value = lowest_random + maximum_change_value + minimum_change_value + randshuffler(5,difference_change_value);
	    }
	    else {
	      new_value = lowest_random + maximum_change_value - minimum_change_value - randshuffler(5,difference_change_value);
	    }
	    //for debugging:
	    //	    printf("\n top_distance: %i bottom_distance: %i lowest_random: %i difference_change_value: %i",top_distance, bottom_distance,lowest_random,difference_change_value);
	    // printf("\n change_polarity: %i new_value: %i",change_polarity,new_value);
		

	    new_buffer[output_buffer_position] = (short int)(new_value);   
	  }
	else
	  {
	    new_buffer[output_buffer_position] = (short int)(buffer_file_contents[input_file_position]);   
	  }

      }

    write_wav(output_file_name, BUF_SIZE, new_buffer, S_RATE);
    printf("\nWrote: %s",output_file_name);

    // close input_file
    if (input_file) { fclose(input_file); }
  }
    return(0);
}

/*creates an entropy pool to make the randomness more random*/
int randshuffler(int pool, int total)
{
     int z, randbuff[pool], randmid, randout;

     for(z=0;z<pool;z++)
     {
          randmid = rnd(total);
          randbuff[z] = (int)(randmid);
     }
     randout = randbuff[rnd(pool)];
     return(randout);
}

/*generates random number in the range given
It is designed for  */
int rnd(int range)
{
     int r1, r2, rtotal;
     r1=rand();
     r2=rand();
     rtotal=(r1+r2)%range;
     return(rtotal);
}

/* seeds the random number using the time*/
void seedrnd(void)
{
     srand((unsigned)time(NULL));
}
