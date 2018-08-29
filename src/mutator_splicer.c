/*mutator_splicer.exe input_file_name_1 input_file_name_2 output_file_name random_seed minimum_size maximum_size

This program splices a random chunk of input_file_name_2 into input_file_name_1 and outputs a new file consisting of this.  The size of the chunk can vary between minimum_size and maximum_size.*/

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
    int minimum_size = atoi(argv[5]);
    int maximum_size = atoi(argv[6]);

     if(argc!=7) { printf("ERROR: Wrong number of arguments.");}
     else if(maximum_size<=minimum_size) {printf("ERROR: Maximum size is not larger than minimum size.");}
     else if(minimum_size<1 || maximum_size > 44099)  {printf("ERROR: Size variables out of range (<1 or >44099).");}
     else
     {
        srand(atoi(argv[4])); // set random seed

        char input_file_name1[40], input_file_name2[40], output_file_name[40];
        short int input_file1_buffer[FILE_SIZE],input_file2_buffer[FILE_SIZE],output_buffer[BUF_SIZE];

         int copy_start_position,copy_end_position,copy_size;
         int paste_start_position, paste_end_position, distance;
         int input_buffer_position; 
        
        //Load input file 1 and copy it into a buffer         
         sprintf(input_file_name1, "%s",argv[1]);
         FILE * input_file1 = fopen(input_file_name1, "rb");
         fread(input_file1_buffer, sizeof(short int), FILE_SIZE,input_file1);

        //Load input file 2 and copy it into a buffer         
         sprintf(input_file_name2, "%s",argv[2]);
         FILE * input_file2 = fopen(input_file_name2, "rb");
         fread(input_file2_buffer, sizeof(short int), FILE_SIZE,input_file2);
         
         // Create a new file named output_file_name
         sprintf(output_file_name, "%s",argv[3]);
         FILE * outfile = fopen(output_file_name, "wb");

        // This calculates the beginning and ending position and sizeof the section of samples copied from input_file2
        copy_size = minimum_size + randshuffler(30,(maximum_size-minimum_size));
        copy_start_position = randshuffler(30,(BUF_SIZE-copy_size));
        copy_end_position = copy_start_position + copy_size;

         //This calculates the place in input_file1 that the section from input_file2 will be pasted.
         paste_start_position = randshuffler(30,(BUF_SIZE-copy_size)); //new starting position
         paste_end_position = paste_start_position + copy_size; // new end position

         //for debugging:
        //printf("copy_size: %i copy_start_position: %i copy_end_position: %i\n", copy_size,copy_start_position,copy_end_position);
        //printf("paste_start_position: %i paste_end_position: %i\n", paste_start_position,paste_end_position);
         
         distance = copy_start_position - paste_start_position;

         for(input_buffer_position = 0; input_buffer_position < BUF_SIZE; input_buffer_position++)
         {
              if (input_buffer_position < paste_start_position || input_buffer_position > paste_end_position)
              {
                    output_buffer[input_buffer_position] = (short int)(input_file1_buffer[(input_buffer_position + HEADER_SIZE)]);
              }
              else
              {
                   output_buffer[input_buffer_position] = (short int)(input_file2_buffer[(input_buffer_position + HEADER_SIZE + distance)]);
              }
         }

         write_wav(output_file_name, BUF_SIZE, output_buffer, S_RATE);
         printf("\nWrote: %s",output_file_name);

          // cleanup
          if (input_file1) { fclose(input_file1); }
          if (input_file2) { fclose(input_file2); }
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
