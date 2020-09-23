/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api.help;

import javax.ws.rs.core.Response.Status;

import org.apache.commons.io.IOUtils;

import eu.internetofus.common.vertx.OperationReponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.api.APIVerticle;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.OperationRequest;
import io.vertx.ext.web.api.OperationResponse;

/**
 * Resource to provide the help about the API.
 *
 * @see Help
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class HelpResource implements Help {

  /**
   * The information about the API.
   */
  protected final APIInfo info;

  /**
   * Name of the resource that contains the Open API description.
   */
  public static final String OPENA_API_RESOURCE = "wenet-interaction_protocol_engine-openapi.yaml";

  /**
   * Create a new version resource.
   *
   * @param apiVerticle where the service will be executed.
   */
  public HelpResource(final APIVerticle apiVerticle) {

    this.info = new APIInfo();
    final var conf = apiVerticle.config().getJsonObject("help", new JsonObject()).getJsonObject("info", new JsonObject());
    this.info.name = conf.getString("name", "wenet/interaction-protocol-engine");
    this.info.apiVersion = conf.getString("apiVersion", "Undefined");
    this.info.softwareVersion = conf.getString("softwareVersion", "Undefined");
    this.info.vendor = conf.getString("vendor", "UDT-IA, IIIA-CSIC");
    this.info.license = conf.getString("license", "MIT");

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void getInfo(final OperationRequest context, final Handler<AsyncResult<OperationResponse>> resultHandler) {

    OperationReponseHandlers.responseOk(resultHandler, this.info);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void getOpenApi(final OperationRequest context, final Handler<AsyncResult<OperationResponse>> resultHandler) {

    Vertx.vertx().executeBlocking((Handler<Promise<String>>) promise -> {

      try {

        final var in = this.getClass().getClassLoader().getResourceAsStream(OPENA_API_RESOURCE);
        final var openapi = IOUtils.toString(in);
        promise.complete(openapi);

      } catch (final Throwable cause) {

        promise.fail(cause);
      }

    }, res -> {

      if (res.failed()) {

        OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.INTERNAL_SERVER_ERROR, "no_read_openapi", "Cannot read the OpenAPI description.");

      } else {

        final String openapi = res.result();
        resultHandler.handle(Future.succeededFuture(new OperationResponse().setStatusCode(Status.OK.getStatusCode()).putHeader(HttpHeaders.CONTENT_TYPE.toString(), "application/yaml").setPayload(Buffer.buffer(openapi))));

      }
    });

  }

}
