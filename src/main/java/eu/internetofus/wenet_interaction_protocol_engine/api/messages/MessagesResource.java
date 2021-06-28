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

package eu.internetofus.wenet_interaction_protocol_engine.api.messages;

import eu.internetofus.common.model.Model;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessage;
import eu.internetofus.common.vertx.ServiceResponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.EngineWorker;
import eu.internetofus.wenet_interaction_protocol_engine.MessageForWorkerBuilder;
import eu.internetofus.wenet_interaction_protocol_engine.ProtocolData;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;
import javax.ws.rs.core.Response.Status;
import org.tinylog.Logger;

/**
 * Implementation of the {@link Messages} services.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class MessagesResource implements Messages {

  /**
   * The event bus that is using.
   */
  protected Vertx vertx;

  /**
   * Create a new instance to provide the services of the {@link Messages}.
   *
   * @param vertx with the event bus to use.
   */
  public MessagesResource(final Vertx vertx) {

    this.vertx = vertx;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void sendMessage(final JsonObject body, final ServiceRequest context,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var message = Model.fromJsonObject(body, ProtocolMessage.class);
    if (message == null) {

      Logger.trace("Fail sendMessage: {} is not a valid JSON.", body);
      ServiceResponseHandlers.responseWithErrorMessage(resultHandler, Status.BAD_REQUEST, "bad_message",
          "The message is not right.");

    } else {

      message.validate("bad_message", this.vertx).onComplete(validation -> {

        if (validation.failed()) {

          final var cause = validation.cause();
          Logger.trace(cause, "Fail sendMessage: {} is not valid.", message);
          ServiceResponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

        } else {

          ProtocolData.createWith(message, this.vertx).onSuccess(protocol -> {
            this.vertx.eventBus().publish(EngineWorker.ADDRESSS,
                MessageForWorkerBuilder.buildProtocolMessage(message, protocol));

          });

          Logger.trace("Accepted sendMessage {}", message);
          ServiceResponseHandlers.responseWith(resultHandler, Status.ACCEPTED, message);
        }

      });

    }
  }

}
