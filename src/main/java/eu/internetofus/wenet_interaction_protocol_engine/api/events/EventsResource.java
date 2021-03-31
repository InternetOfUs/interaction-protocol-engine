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

package eu.internetofus.wenet_interaction_protocol_engine.api.events;

import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolEvent;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.vertx.ServiceResponseHandlers;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;
import java.util.concurrent.TimeUnit;
import javax.ws.rs.core.Response.Status;
import org.tinylog.Logger;

/**
 * Implementation of the {@link Events} services.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class EventsResource implements Events {

  /**
   * The event bus that is using.
   */
  protected Vertx vertx;

  /**
   * Create a new instance to provide the services of the {@link Events}.
   *
   * @param vertx with the event bus to use.
   */
  public EventsResource(final Vertx vertx) {

    this.vertx = vertx;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void sendEvent(final JsonObject body, final ServiceRequest context,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var event = Model.fromJsonObject(body, ProtocolEvent.class);
    if (event == null) {

      Logger.trace("Fail sendEvent: {} is not a valid JSON.", body);
      ServiceResponseHandlers.responseWithErrorMessage(resultHandler, Status.BAD_REQUEST, "bad_event",
          "The event is not right.");

    } else {

      event.validate("bad_event", this.vertx).onComplete(validation -> {

        if (validation.failed()) {

          final var cause = validation.cause();
          Logger.trace(cause, "Fail sendEvent: {} is not valid.", event);
          ServiceResponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

        } else {

          event.id = this.vertx.setTimer(TimeUnit.SECONDS.toMillis(event.delay), id -> {

            final var message = event.toProtocolMessage();
            WeNetInteractionProtocolEngine.createProxy(this.vertx).sendMessage(message);
          });

          Logger.trace("Accepted sendEvent {}", event);
          ServiceResponseHandlers.responseWith(resultHandler, Status.ACCEPTED, event);
        }

      });

    }
  }

  @Override
  public void deleteEvent(final long id, final ServiceRequest context,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    if (this.vertx.cancelTimer(id)) {

      Logger.trace("Cancelled event {}", id);
      ServiceResponseHandlers.responseOk(resultHandler);

    } else {

      Logger.trace("Cannot cancel event {}, because not found event", id);
      ServiceResponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_FOUND, "bad_event.id",
          "Cannot delete the event because not found any with the specified identifier.");

    }
  }

}
