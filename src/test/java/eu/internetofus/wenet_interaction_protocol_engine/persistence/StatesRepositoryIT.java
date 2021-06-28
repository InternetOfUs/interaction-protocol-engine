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

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.model.Model;
import eu.internetofus.common.components.interaction_protocol_engine.State;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration test over the {@link StatesRepository}.
 *
 * @see StatesRepository
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class StatesRepositoryIT {

  /**
   * Verify that can not found a state if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#searchState(String, String,String)
   */
  @Test
  public void shouldNotFoundUndefinedState(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertFailure(StatesRepository.createProxy(vertx).searchState("undefined", "undefined", "undefined"))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Create an state that is unique.
   *
   * @return the created state.
   */
  protected State createUniqueState() {

    var state = new State();
    state.attributes = new JsonObject().put("key", "value");
    state.communityId = UUID.randomUUID().toString();
    state.taskId = UUID.randomUUID().toString();
    state.userId = UUID.randomUUID().toString();
    return state;

  }

  /**
   * Verify that can found a state.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#searchState(String, String,String)
   */
  @Test
  public void shouldFoundState(final Vertx vertx, final VertxTestContext testContext) {

    var state = this.createUniqueState();
    testContext.assertComplete(StatesRepository.createProxy(vertx).storeState(state)).onSuccess(storedState -> {
      testContext.assertComplete(
          StatesRepository.createProxy(vertx).searchState(state.communityId, state.taskId, state.userId)).onSuccess(

              foundState -> testContext.verify(() -> {
                assertThat(foundState).isEqualTo(storedState);
                testContext.completeNow();
              }));
    });

  }

  /**
   * Verify that can not store a state that can not be an object.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#storeState(State)
   */
  @Test
  public void shouldNotStoreAStateThatCanNotBeAnObject(final Vertx vertx, final VertxTestContext testContext) {

    final State state = new State() {

      /**
       * {@inheritDoc}
       */
      @Override
      public JsonObject toJsonObject() {

        return null;
      }
    };
    testContext.assertFailure(StatesRepository.createProxy(vertx).storeState(state))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Verify that can store a state.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#storeState(State)
   */
  @Test
  public void shouldStoreState(final Vertx vertx, final VertxTestContext testContext) {

    var state = this.createUniqueState();
    testContext.assertComplete(StatesRepository.createProxy(vertx).storeState(state))
        .onSuccess(storedState -> testContext.verify(() -> {

          assertThat(storedState).isEqualTo(state);
          testContext.completeNow();
        }));

  }

  /**
   * Verify that can not update a state if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#updateState(State)
   */
  @Test
  public void shouldNotUpdateUndefinedState(final Vertx vertx, final VertxTestContext testContext) {

    var state = this.createUniqueState();
    testContext.assertFailure(StatesRepository.createProxy(vertx).updateState(state))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Verify that can not update a state if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#updateState(State)
   */
  @Test
  public void shouldNotUpdateAStateThatCanNotBeAnObject(final Vertx vertx, final VertxTestContext testContext) {

    final State state = new State() {

      /**
       * {@inheritDoc}
       */
      @Override
      public JsonObject toJsonObject() {

        return null;
      }
    };
    testContext.assertFailure(StatesRepository.createProxy(vertx).updateState(state))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Verify that can update a state.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#updateState(State)
   */
  @Test
  public void shouldUpdateState(final Vertx vertx, final VertxTestContext testContext) {

    var state = this.createUniqueState();
    testContext.assertComplete(StatesRepository.createProxy(vertx).storeState(state))
        .onSuccess(stored -> testContext.verify(() -> {

          final var update = Model.fromJsonObject(stored.toJsonObject(), State.class);
          update.attributes.put("key2", "value2");
          testContext.assertComplete(StatesRepository.createProxy(vertx).updateState(update).compose(
              empty -> StatesRepository.createProxy(vertx).searchState(state.communityId, state.taskId, state.userId)))
              .onSuccess(foundState -> testContext.verify(() -> {

                assertThat(foundState._creationTs).isEqualTo(stored._creationTs);
                assertThat(foundState._lastUpdateTs).isGreaterThanOrEqualTo(stored._lastUpdateTs);
                update._creationTs = stored._creationTs;
                update._lastUpdateTs = foundState._lastUpdateTs;
                assertThat(foundState).isEqualTo(update);
                testContext.completeNow();
              }));

        }));

  }

}
