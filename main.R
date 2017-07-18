library( httpuv )
library( jsonlite )

server <- NULL
websocket <- NULL

webApp <- list(
   
   call = function( req ) {
      list(
         status = 200L,
         headers = list( 'Content-Type' = 'text/html' ),
         body = paste( readLines( "page.html" ), collapse="\n" )
      )
   },
   
   onWSOpen = function( ws ) {
      if( ws$request$HTTP_SEC_WEBSOCKET_PROTOCOL != "RJS-0" )
         stop( paste( "Error in rjs: Unknown WebSocket protocol", ws$request$HTTP_SEC_WEBSOCKET_PROTOCOL ) )
      ws$onMessage( function( isBinary, msg ) {
         if( isBinary )
            stop( "Error in rjs: Binary message received via WebSocket" )
         msg <- fromJSON( msg )
         if( !is.character( msg[1] ) ) {
            stop( "Error in rjs: Malformed message received via WebSocket" )
         }
         if( msg[1] == "REPLY_JS" ) {
            result_callbacks[[ msg[2] ]]( fromJSON( msg[[3]] ) )
            rm( list=msg[2], envir = result_callbacks )
         } else if( msg[1] == "REPLY_JS_ERROR" ) {
            if( msg[2] != "-1" ) {
               rm( list=msg[2], envir = result_callbacks ) 
            }
            cat( paste( "rjs: JavaScript error:", msg[3] ) )
         } else
            stop( "Error in rjs: Unknown opcode received via WebSocket" )
      } );
      websocket <<- ws
   }
   
)

max_result_handle <- 0L

result_callbacks <- new.env()

evaljs <- function( jscode, onresult=NULL ) {
   if( !is.null(onresult) ) {
      max_result_handle <<- max_result_handle + 1
      result_handle <- as.character( max_result_handle )
      result_callbacks[[ result_handle ]] <- onresult
   } else {
      result_handle <- -1L
   }
   websocket$send( sprintf( "[\"EVAL_JS\", %s, %s ]", result_handle, deparse(jscode) ) )
}

calljs <- function( .function, ..., isJSON=FALSE, onresult=NULL ) {
   if( !is.null(onresult) ) {
      max_result_handle <<- max_result_handle + 1
      result_handle <- as.character( max_result_handle )
      result_callbacks[[ result_handle ]] <- onresult
   } else {
      result_handle <- -1L
   }
   if( isJSON ) {
      args <- deparse( paste0( "[", paste( ..., sep=", " ), "]" ) )
   } else {
      args <- toJSON( lapply( list(...), unbox ) )
   }
   print( args )
   websocket$send( sprintf( "[\"CALL_JS\", %s, \"%s\", %s ]", 
      result_handle, .function, args ) )
}

if( !is.null(server) ){
   stopDaemonizedServer( server )
   server <- NULL
   websocket <- NULL
}
server <- startDaemonizedServer( "0.0.0.0", 1235, webApp )
getOption("viewer")( "http://localhost:1235" )
while( is.null(websocket) ) {
   Sys.sleep( .05 )
}

evaljs( 'Math.sin(3)', print ) 
calljs( 'log', 'AB', 3, isJSON=FALSE )

evaljs( 'add = function( x, y ) { return [ x+y, 0 ] }' )
calljs( 'add', 3, 5, onresult=print )

#stopDaemonizedServer( server )
#server <- NULL
#websocket <- NULL
